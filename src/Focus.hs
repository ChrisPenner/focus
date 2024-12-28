{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Focus (run) where

import Control.Applicative
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.State
import Control.Monad.Trans.Resource (runResourceT)
import Data.Bifunctor
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Error.Diagnose qualified as D
import Error.Diagnose qualified as Diagnose
import Focus.Cli (Alignment (..), InPlace (..), Options (..), OutputLocation (..), ShowWarnings (..), UseColour (..), optionsP)
import Focus.Command (Command (..), CommandF (..), CommandT (..))
import Focus.Compile (compileSelector)
import Focus.Exec qualified as Exec
import Focus.Parser (parseSelector)
import Focus.Prelude
import Focus.Typechecker (typecheckModify)
import Focus.Typechecker.Types qualified as Types
import Focus.Types
import Focus.Untyped (renderChunk)
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
  opts@Options {command = Modify script inputFiles, output, chunkSize, inPlace, alignMode} <-
    Opts.customExecParser
      (Opts.prefs (Opts.subparserInline <> Opts.showHelpOnError <> Opts.disambiguate <> Opts.showHelpOnEmpty <> Opts.helpShowGlobals))
      ( Opts.info
          (optionsP <**> Opts.helper)
          ( Opts.fullDesc
              <> Opts.progDesc "Focus - cli utility for hacking and slashing data"
          )
      )
  cliStateVar <- newTVarIO $ CliState mempty
  flip runReaderT (cliStateVar, opts) . unCli $ do
    focus <- getModifyFocus inputFiles script
    case (inputFiles, alignMode, inPlace) of
      ([], _, InPlace) -> do
        failWith "In-place mode specified, but no input files provided."
      ([], _, NotInPlace) -> do
        let getOutput =
              case output of
                StdOut -> pure IO.stdout
                OutputFile path -> IO.openFile path IO.WriteMode
        IO.bracket getOutput IO.hClose \outputHandle -> do
          focusMToCliM $ do
            Exec.runModify focus chunkSize IO.stdin outputHandle
      (_, Unaligned, _) -> do
        for_ inputFiles \inputFile -> do
          let withOutput go =
                case inPlace of
                  NotInPlace -> do
                    case output of
                      StdOut -> go IO.stdout
                      OutputFile path -> IO.withFile path IO.WriteMode go
                  InPlace -> do
                    UnliftIO.withSystemTempFile "focus.txt" \tempPath tempHandle -> do
                      _ <- go tempHandle
                      UnliftIO.renameFile tempPath inputFile
                      pure ()
          withOutput \outputHandle -> do
            IO.withFile inputFile IO.ReadMode \inputHandle -> do
              focusMToCliM $ do
                Exec.runModify focus chunkSize inputHandle outputHandle
      (_, Aligned, InPlace) ->
        failWith "Cannot use both aligned and in-place modes together."
      (_, Aligned, _) -> do
        let openInputs = traverse (\fp -> IO.openFile fp IO.ReadMode) inputFiles
        let withInputs = IO.bracket openInputs (traverse IO.hClose)
        withInputs \inputHandles -> do
          let getOutput = case output of
                StdOut -> pure IO.stdout
                OutputFile path -> IO.openFile path IO.WriteMode
          IO.bracket getOutput IO.hClose \outputHandle -> do
            focusMToCliM $ do
              Exec.runAligned focus chunkSize inputHandles outputHandle
  where
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

    getModifyFocus :: [FilePath] -> Text -> CliM (Focus ViewT Chunk Chunk)
    getModifyFocus inputFiles selectorTxt = do
      addSource "<selector>" selectorTxt
      case parseSelector "<selector>" selectorTxt of
        Left errDiagnostic -> do
          failWithDiagnostic errDiagnostic
        Right selectorAst -> do
          alignment <- asks alignMode
          case typecheckModify (initVars alignment inputFiles) selectorAst of
            Left errReport -> failWithReport errReport
            Right warnings -> do
              printWarnings warnings
              compiledSel <- liftIO $ compileSelector ViewF selectorAst
              pure compiledSel
    initVars :: Alignment -> [FilePath] -> Map Text (Typ s)
    initVars alignment inputFiles = case alignment of
      Aligned -> Map.fromList $ zipWith const ([(1 :: Int) ..] <&> (\i -> ("f" <> Text.pack (show i), Types.textType argPosition))) inputFiles
      Unaligned -> mempty

    argPosition :: D.Position
    argPosition =
      -- TODO: fix this
      D.Position (0, 0) (0, 0) "unknown"

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
