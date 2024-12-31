{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Focus (run) where

import Control.Applicative
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.State
import Control.Monad.Trans.Resource (runResourceT)
import Data.Bifunctor
import Data.Functor.Identity
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Error.Diagnose qualified as D
import Error.Diagnose qualified as Diagnose
import Focus.Cli (Alignment (..), InPlace (..), Options (..), ShowWarnings (..), UseColour (..), optionsP)
import Focus.Command (Command (..), CommandF (..), CommandT (..), InputLocation (..), OutputLocation (..), ScriptLocation (..))
import Focus.Compile (compileSelector)
import Focus.Exec qualified as Exec
import Focus.Parser (parseSelector)
import Focus.Prelude
import Focus.Typechecker (typecheckModify)
import Focus.Typechecker.Types qualified as Types
import Focus.Types
import Focus.Untyped (BindingSymbol (..), renderChunk)
import Options.Applicative qualified as Opts
import Prettyprinter.Render.Terminal (AnsiStyle)
import System.Exit qualified as System
import UnliftIO qualified as IO
import UnliftIO.Directory qualified as UnliftIO
import UnliftIO.IO qualified as UnliftIO
import UnliftIO.STM
import UnliftIO.Temporary qualified as UnliftIO

data CliState = CliState
  { diagnostic :: D.Diagnostic Text
  }

addSource :: Text -> Text -> CliM ()
addSource name src = do
  modify \s -> s {diagnostic = D.addFile (diagnostic s) (Text.unpack name) (Text.unpack src)}

newtype CliM a = CliM {unCli :: ReaderT (TVar CliState, Options) IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, IO.MonadUnliftIO)

instance MonadReader Options CliM where
  ask = CliM $ asks snd
  local f (CliM m) = CliM $ local (second f) m

instance MonadState (CliState) CliM where
  get = CliM $ asks fst >>= liftIO . readTVarIO
  put s = CliM $ asks fst >>= \tv -> liftIO $ atomically $ writeTVar tv s

run :: IO ()
run = do
  opts@Options {command = Command scriptLoc inputLocs, output, chunkSize, inPlace, alignMode} <-
    Opts.customExecParser
      (Opts.prefs (Opts.subparserInline <> Opts.showHelpOnError <> Opts.disambiguate <> Opts.showHelpOnEmpty <> Opts.helpShowGlobals))
      ( Opts.info
          (optionsP <**> Opts.helper)
          ( Opts.fullDesc
              <> Opts.progDesc "Focus - cli utility for hacking and slashing data"
          )
      )
  (scriptName, script) <- resolveScript scriptLoc
  cliStateVar <- newTVarIO $ CliState mempty
  flip runReaderT (cliStateVar, opts) . unCli $ do
    focus <- getModifyFocus inputLocs scriptName script
    case (inputLocs, alignMode, inPlace) of
      ([], _, InPlace) -> do
        failWith "In-place mode specified, but no input files provided."
      ([], _, NotInPlace) -> do
        let getOutput =
              case output of
                StdOut -> pure IO.stdout
                OutputFile path -> IO.openFile path IO.WriteMode
        IO.bracket getOutput IO.hClose \outputHandle -> do
          focusMToCliM $ do
            Exec.runModify focus chunkSize Nothing outputHandle
      (_, Unaligned, _) -> do
        let hasStdIn =
              inputLocs & any \case
                StdIn -> True
                _ -> False
        if hasStdIn && inPlace == InPlace
          then failWith "Cannot pass stdin as input when in-place mode is requested."
          else pure ()
        for_ inputLocs \inputLoc -> do
          let withOutput go =
                case inPlace of
                  NotInPlace -> do
                    case output of
                      StdOut -> go IO.stdout
                      OutputFile path -> IO.withFile path IO.WriteMode go
                  InPlace -> do
                    UnliftIO.withSystemTempFile "focus.txt" \tempPath tempHandle -> do
                      _ <- go tempHandle
                      case inputLoc of
                        StdIn -> failWith "Cannot use stdin as input when in-place mode is requested."
                        InputFile inputFile -> do
                          UnliftIO.renameFile tempPath inputFile
                          pure ()
          withOutput \outputHandle -> do
            withInputs (Identity inputLoc) \(Identity inputHandle) -> do
              focusMToCliM $ do
                Exec.runModify focus chunkSize (Just inputHandle) outputHandle
      (_, Aligned, InPlace) ->
        failWith "Cannot use both aligned and in-place modes together."
      (_, Aligned, _) -> do
        withInputs inputLocs \inputHandles -> do
          let getOutput = case output of
                StdOut -> pure IO.stdout
                OutputFile path -> IO.openFile path IO.WriteMode
          IO.bracket getOutput IO.hClose \outputHandle -> do
            focusMToCliM $ do
              Exec.runAligned focus chunkSize inputHandles outputHandle
  where
    openLoc :: (MonadIO m) => InputLocation -> m (Either IO.Handle IO.Handle)
    openLoc = \case
      StdIn -> pure (Left IO.stdin)
      InputFile path -> Right <$> IO.openFile path IO.ReadMode
    withInputs :: (Traversable f) => f InputLocation -> (f IO.Handle -> CliM a) -> CliM a
    withInputs inputLocs f = do
      let acquire = traverse openLoc inputLocs
      let release = (traverse . traverse) IO.hClose
      IO.bracket acquire release (f . fmap (either id id))
    failWithDiagnostic :: Diagnose.Diagnostic Text -> CliM a
    failWithDiagnostic diagnostic = do
      style <- diagnoseStyle
      printDiagnostic style diagnostic
      liftIO $ System.exitFailure

    failWithReport :: Diagnose.Report Text -> CliM a
    failWithReport report = do
      diag <- gets diagnostic
      failWithDiagnostic $ Diagnose.addReport diag report

    failWith :: (MonadIO m) => Text -> m a
    failWith msg = do
      liftIO $ TextIO.hPutStrLn UnliftIO.stderr msg
      liftIO $ System.exitFailure

    getModifyFocus :: [InputLocation] -> Text -> Text -> CliM (Focus ViewT Chunk Chunk)
    getModifyFocus inputFiles selectorName selectorTxt = do
      let inputTyp = case inputFiles of
            [] -> Types.nullType argPosition
            _ -> Types.textType argPosition
      addSource selectorName selectorTxt
      case parseSelector selectorName selectorTxt of
        Left errDiagnostic -> do
          failWithDiagnostic errDiagnostic
        Right selectorAst -> do
          alignment <- asks alignMode
          case typecheckModify (initVars alignment inputFiles) inputTyp selectorAst of
            Left errReport -> failWithReport errReport
            Right warnings -> do
              printWarnings warnings
              compiledSel <- liftIO $ compileSelector ViewF selectorAst
              pure compiledSel
    initVars :: Alignment -> [InputLocation] -> Map BindingSymbol Typ
    initVars alignment inputFiles = case alignment of
      Aligned -> Map.fromList $ zipWith const ([(1 :: Int) ..] <&> (\i -> (BindingSymbol $ "f" <> Text.pack (show i), Types.textType argPosition))) inputFiles
      Unaligned -> mempty

    argPosition :: D.Position
    argPosition =
      -- TODO: fix this
      D.Position (0, 0) (0, 0) "unknown"

    resolveScript :: (MonadIO m) => ScriptLocation -> m (Text, Text)
    resolveScript = \case
      ScriptLiteral txt -> pure ("<argument>", txt)
      ScriptFile path -> do
        contents <- liftIO $ TextIO.readFile path
        pure (Text.pack path, contents)

printWarnings :: [WarningReport] -> CliM ()
printWarnings reports = do
  asks showWarnings >>= \case
    ShowWarnings -> do
      for_ reports printReport
    NoWarnings -> pure ()

printReport :: D.Report Text -> CliM ()
printReport report = do
  diag <- gets diagnostic
  style <- diagnoseStyle
  printDiagnostic style $ Diagnose.addReport diag report

focusMToCliM :: FocusM a -> CliM a
focusMToCliM m = do
  env <- mkEnv
  liftIO . runResourceT . flip runReaderT env . runFocusM $ m
  where
    mkEnv :: CliM FocusEnv
    mkEnv = do
      style <- diagnoseStyle
      d <- gets diagnostic
      pure $
        FocusEnv
          { _focusBindings = mempty,
            _focusOpts = FocusOpts {_handleErr = handleError style d True}
          }

    handleError :: Diagnose.Style AnsiStyle -> D.Diagnostic Text -> Bool -> SelectorError -> IO ()
    handleError style diag shouldFail err = do
      case err of
        ShellError pos msg -> do
          let report =
                D.Err
                  Nothing
                  "Shell error"
                  [ (pos, D.This $ "stderr:\n" <> msg)
                  ]
                  []
          printDiagnostic style $ D.addReport diag report
        BindingError pos msg -> do
          let report =
                D.Err
                  Nothing
                  "Binding error"
                  [ (pos, D.This $ msg)
                  ]
                  []
          printDiagnostic style $ D.addReport diag report
        JsonParseError pos txt e -> do
          let report =
                D.Err
                  Nothing
                  "JSON parse error"
                  [ (pos, D.This $ Text.unlines ["Encountered invalid JSON.", "Error: " <> e, "Value: " <> txt])
                  ]
                  []
          printDiagnostic style $ D.addReport diag report
        CastError pos ty chunk -> do
          let report =
                D.Err
                  Nothing
                  "Type cast error"
                  [ (pos, D.This $ "Failed to cast value: " <> renderChunk chunk <> " to type: " <> Text.pack (show ty))
                  ]
                  []
          printDiagnostic style $ D.addReport diag report
        MathError pos msg -> do
          let report =
                D.Err
                  Nothing
                  "Math error"
                  [ (pos, D.This $ msg)
                  ]
                  []
          printDiagnostic style $ D.addReport diag report
      if shouldFail
        then liftIO $ System.exitFailure
        else pure ()

printDiagnostic :: (MonadIO m) => Diagnose.Style AnsiStyle -> D.Diagnostic Text -> m ()
printDiagnostic style diag = do
  Diagnose.printDiagnostic UnliftIO.stderr Diagnose.WithUnicode (Diagnose.TabSize 2) style diag

diagnoseStyle :: (MonadReader Options m) => m (Diagnose.Style AnsiStyle)
diagnoseStyle =
  asks useColour <&> \case
    Colour -> Diagnose.defaultStyle
    NoColour -> Diagnose.unadornedStyle
