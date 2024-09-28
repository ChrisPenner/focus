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
import Focus.Cli (InPlace (..), Options (..), OutputLocation (..), UseColour (..), optionsP)
import Focus.Command (Command (..), CommandF (..), CommandT (..), IsCmd)
import Focus.Compile (compileAction, compileSelector)
import Focus.Exec qualified as Exec
import Focus.Parser (parseAction, parseSelector)
import Focus.Prelude
import Focus.Typechecker (typecheckModify, typecheckSelector, typecheckView)
import Focus.Types
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
      View script inputFiles -> withHandles command inPlace inputFiles output \inputHandle outputHandle -> flip evalStateT (CliState mempty) do
        focus <- getActionFocus script
        focusMToCliM $ Exec.runView focus chunkSize inputHandle outputHandle
      Modify script m inputFiles -> withHandles command inPlace inputFiles output \inputHandle outputHandle -> flip evalStateT (CliState mempty) do
        (focus, action) <- getModifyFocus script m
        focusMToCliM $ Exec.runModify focus action chunkSize inputHandle outputHandle
      Set script val inputFiles -> withHandles command inPlace inputFiles output \inputHandle outputHandle -> flip evalStateT (CliState mempty) do
        focus <- getSelectorFocus ModifyF script
        focusMToCliM $ Exec.runSet focus chunkSize inputHandle outputHandle val
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
        (View {}, InPlace, _) -> failWith "In-place mode not supported for view command."
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

    getSelectorFocus :: (IsCmd cmd) => CommandF cmd -> Text -> CliM (Focus cmd Chunk Chunk)
    getSelectorFocus cmdF selectorTxt = do
      addSource "<selector>" selectorTxt
      case parseSelector "<selector>" selectorTxt of
        Left errDiagnostic -> do
          failWithDiagnostic errDiagnostic
        Right ast -> do
          case typecheckSelector ast of
            Left errReport -> failWithReport errReport
            Right () -> do
              pure $ compileSelector cmdF ast

    getActionFocus :: Text -> CliM (Focus ViewT Chunk Chunk)
    getActionFocus actionTxt = do
      addSource "<action>" actionTxt
      case parseAction "<action>" actionTxt of
        Left errDiagnostic -> do
          failWithDiagnostic errDiagnostic
        Right ast -> do
          case typecheckView ast of
            Left errReport -> failWithReport errReport
            Right () -> do
              pure $ compileAction ast

    getModifyFocus :: Text -> Text -> CliM (Focus ModifyT Chunk Chunk, Focus ViewT Chunk Chunk)
    getModifyFocus selectorTxt actionTxt = do
      addSource "<selector>" selectorTxt
      addSource "<action>" actionTxt
      case parseSelector "<selector>" selectorTxt of
        Left errDiagnostic -> do
          failWithDiagnostic errDiagnostic
        Right selectorAst -> do
          case parseAction "<action>" actionTxt of
            Left errDiagnostic -> do
              failWithDiagnostic errDiagnostic
            Right action -> do
              case typecheckModify selectorAst action of
                Left errReport -> failWithReport errReport
                Right () -> do
                  let compiledSel = compileSelector ModifyF selectorAst
                  let compiledAction = compileAction action
                  pure $ (compiledSel, compiledAction)

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

    handleError :: Diagnose.Style AnsiStyle -> D.Diagnostic Text -> Bool -> Chunk -> SelectorError -> IO Chunk
    handleError style diag shouldFail chunk err = do
      case err of
        ShellError msg -> do
          liftIO $ TextIO.hPutStrLn UnliftIO.stderr msg
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
      if shouldFail
        then liftIO $ System.exitFailure
        else pure chunk

printDiagnostic :: (MonadIO m) => Diagnose.Style AnsiStyle -> D.Diagnostic Text -> m ()
printDiagnostic style report = do
  Diagnose.printDiagnostic UnliftIO.stderr Diagnose.WithUnicode (Diagnose.TabSize 2) style report

diagnoseStyle :: (MonadReader Options m) => m (Diagnose.Style AnsiStyle)
diagnoseStyle =
  asks useColour <&> \case
    Colour -> Diagnose.defaultStyle
    NoColour -> Diagnose.unadornedStyle
