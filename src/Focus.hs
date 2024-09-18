module Focus (run) where

import Control.Applicative
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (ReaderT (..), asks)
import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Error.Diagnose qualified as D
import Error.Diagnose qualified as Diagnose
import Focus.Cli (InPlace (..), Options (..), OutputLocation (..), UseColour (..), optionsP)
import Focus.Command (Command (..), CommandF (..))
import Focus.Compile (Focus, FocusM (..), SelectorError (..), compileSelector)
import Focus.Debug (debugM)
import Focus.Exec qualified as Exec
import Focus.Parser (parseScript)
import Focus.Prelude
import Focus.Typechecker (typecheckSelector)
import Focus.Typechecker.Types (SomeTypedSelector (..))
import Options.Applicative qualified as Opts
import Prettyprinter.Render.Terminal (AnsiStyle)
import System.Exit qualified as System
import UnliftIO (Handle, MonadUnliftIO)
import UnliftIO qualified as IO
import UnliftIO.Directory qualified as UnliftIO
import UnliftIO.IO qualified as UnliftIO
import UnliftIO.Temporary qualified as UnliftIO

data CliState = CliState
  { sources :: Map Text Text
  }

addSource :: Text -> Text -> CliM ()
addSource name src = do
  modify \s -> s {sources = Map.insert name src (sources s)}

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
      View script inputFiles -> withHandles inPlace inputFiles output \inputHandle outputHandle -> flip evalStateT (CliState mempty) do
        addSource "<selector>" script
        focus <- getFocus "<selector>" ViewF script
        r <- liftIO . flip runReaderT mempty . runExceptT . runFocusM $ Exec.runView focus chunkSize inputHandle outputHandle
        handleError r
      Modify script m inputFiles -> withHandles inPlace inputFiles output \inputHandle outputHandle -> flip evalStateT (CliState mempty) do
        addSource "<selector>" script
        focus <- getFocus "<selector>" ModifyF script
        addSource "<modifier>" m
        modifier <- getFocus "<modifier>" ModifyF m
        r <- liftIO . flip runReaderT mempty . runExceptT . runFocusM $ Exec.runModify focus modifier chunkSize inputHandle outputHandle
        handleError r
      Set script val inputFiles -> withHandles inPlace inputFiles output \inputHandle outputHandle -> flip evalStateT (CliState mempty) do
        addSource "<selector>" script
        focus <- getFocus "<selector>" ModifyF script
        r <- liftIO . flip runReaderT mempty . runExceptT . runFocusM $ Exec.runSet focus chunkSize inputHandle outputHandle val
        handleError r
  where
    handleError :: Either SelectorError () -> CliM ()
    handleError = \case
      Left err -> case err of
        ShellError msg -> do
          liftIO $ TextIO.hPutStrLn UnliftIO.stderr msg
          liftIO $ System.exitFailure
        BindingError pos msg -> do
          let report =
                D.Err
                  Nothing
                  "Binding error"
                  [ (pos, D.This $ msg)
                  ]
                  []
          failWithReport report
      Right () -> pure ()
    diagnoseStyle :: CliM (Diagnose.Style AnsiStyle)
    diagnoseStyle =
      asks useColour <&> \case
        Colour -> Diagnose.defaultStyle
        NoColour -> Diagnose.unadornedStyle
    failWithDiagnostic :: Diagnose.Diagnostic Text -> CliM a
    failWithDiagnostic diagnostic = do
      style <- diagnoseStyle
      Diagnose.printDiagnostic UnliftIO.stderr Diagnose.WithUnicode (Diagnose.TabSize 2) style diagnostic
      liftIO $ System.exitFailure

    failWithReport :: Diagnose.Report Text -> CliM a
    failWithReport report = do
      CliState srcs <- get
      let diagnostic = Map.toList srcs & foldMap \(srcName, src) -> Diagnose.addFile mempty (Text.unpack srcName) (Text.unpack src)
      failWithDiagnostic $ Diagnose.addReport diagnostic report

    failWith :: (MonadIO m) => Text -> m a
    failWith msg = do
      liftIO $ TextIO.hPutStrLn UnliftIO.stderr msg
      liftIO $ System.exitFailure

    withHandles :: forall m. (MonadUnliftIO m) => InPlace -> [FilePath] -> OutputLocation -> (IO.Handle -> IO.Handle -> m ()) -> m ()
    withHandles inPlace inputFiles output action = do
      case (inPlace, inputFiles) of
        (InPlace, []) -> failWith "In-place mode specified, but no input files provided."
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

    getFocus :: Text -> CommandF cmd -> Text -> CliM (Focus cmd FocusM)
    getFocus srcName cmdF script = do
      case parseScript srcName script of
        Left errDiagnostic -> do
          style <- diagnoseStyle
          Diagnose.printDiagnostic UnliftIO.stderr Diagnose.WithUnicode (Diagnose.TabSize 2) style errDiagnostic
          liftIO $ System.exitFailure
        Right ast -> do
          debugM "Selector" ast
          case typecheckSelector ast of
            Left errReport -> failWithReport errReport
            Right (SomeTypedSelector typedSelector) -> do
              pure $ compileSelector cmdF typedSelector
