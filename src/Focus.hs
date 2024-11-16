{-# LANGUAGE DataKinds #-}

module Focus (run) where

import Control.Applicative
import Control.Monad.Reader (MonadReader, ReaderT (..), asks)
import Control.Monad.State
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Error.Diagnose qualified as D
import Error.Diagnose qualified as Diagnose
import Focus.Cli (InPlace (..), Options (..), OutputLocation (..), ShowWarnings (..), UseColour (..), optionsP)
import Focus.Command (Command (..), CommandF (..), CommandT (..))
import Focus.Compile (compileSelector)
import Focus.Exec qualified as Exec
import Focus.Parser (parseSelector)
import Focus.Prelude
import Focus.Typechecker (typecheckModify)
import Focus.Types
import Focus.Untyped (renderChunk)
import Options.Applicative qualified as Opts
import Prettyprinter.Render.Terminal (AnsiStyle)
import System.Exit qualified as System
import UnliftIO (Handle, MonadUnliftIO)
import UnliftIO qualified as IO
import UnliftIO.Directory qualified as UnliftIO
import UnliftIO.IO qualified as UnliftIO
import UnliftIO.Temporary qualified as UnliftIO

data CliState = CliState
  { diagnostic :: D.Diagnostic Text
  }

addSource :: Text -> Text -> CliM ()
addSource name src = do
  modify \s -> s {diagnostic = D.addFile (diagnostic s) (Text.unpack name) (Text.unpack src)}

type CliM = StateT CliState (ReaderT Options IO)

run :: IO ()
run = do
  opts@Options {command, output, chunkSize, inPlace} <-
    Opts.customExecParser
      (Opts.prefs (Opts.subparserInline <> Opts.showHelpOnError <> Opts.disambiguate <> Opts.showHelpOnEmpty <> Opts.helpShowGlobals))
      ( Opts.info
          (optionsP <**> Opts.helper)
          ( Opts.fullDesc
              <> Opts.progDesc "Focus - cli utility for hacking and slashing data"
          )
      )
  flip runReaderT opts do
    case command of
      Modify script inputFiles -> withHandles command inPlace inputFiles output \inputHandle outputHandle -> flip evalStateT (CliState mempty) do
        focus <- getModifyFocus script
        focusMToCliM $ Exec.runView focus chunkSize inputHandle outputHandle
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

    withHandles :: forall m. (MonadUnliftIO m) => Command -> InPlace -> [FilePath] -> OutputLocation -> (IO.Handle -> IO.Handle -> m ()) -> m ()
    withHandles cmd inPlace inputFiles output action = do
      case (cmd, inPlace, inputFiles) of
        (_, InPlace, []) -> failWith "In-place mode specified, but no input files provided."
        _ -> do
          for_ inputFiles \inpFile -> do
            exists <- UnliftIO.doesFileExist inpFile
            when (not exists) do
              failWith $ "Input file does not exist: " <> (Text.pack inpFile)
          let withInputHandler :: (Either (FilePath, Handle) Handle -> m ()) -> m ()
              withInputHandler = case inputFiles of
                [] -> \go -> go (Right IO.stdin)
                _ -> \go -> do
                  for_ inputFiles \inputFile -> do
                    IO.withFile inputFile IO.ReadMode \inputHandle -> go $ Left (inputFile, inputHandle)
          let withOutputHandle :: Maybe FilePath -> ((UnliftIO.Handle -> m ()) -> m ())
              withOutputHandle inputFile = case (inputFile, inPlace) of
                (_, NotInPlace) -> case output of
                  StdOut -> \go -> go IO.stdout
                  OutputFile path -> \go -> do
                    IO.withFile path IO.WriteMode (\outputHandle -> go outputHandle)
                (Just inputFilePath, InPlace) -> \go -> do
                  UnliftIO.withSystemTempFile "focus.txt" \tempPath tempHandle -> do
                    r <- go tempHandle
                    UnliftIO.renameFile tempPath inputFilePath
                    pure r
                _ -> \_ -> failWith "Cannot modify stdin in-place"
          withInputHandler \input -> do
            withOutputHandle (either (Just . fst) (const Nothing) input) \outputHandle ->
              action (either snd id input) outputHandle

    getModifyFocus :: Text -> CliM (Focus ViewT Chunk Chunk)
    getModifyFocus selectorTxt = do
      addSource "<selector>" selectorTxt
      case parseSelector "<selector>" selectorTxt of
        Left errDiagnostic -> do
          failWithDiagnostic errDiagnostic
        Right selectorAst -> do
          case typecheckModify selectorAst of
            Left errReport -> failWithReport errReport
            Right warnings -> do
              printWarnings warnings
              compiledSel <- liftIO $ compileSelector ViewF selectorAst
              pure compiledSel

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
  liftIO . flip runReaderT env . runFocusM $ m
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
