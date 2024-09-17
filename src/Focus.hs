module Focus (run) where

import Control.Applicative
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (MonadIO (liftIO), ReaderT (..), asks)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Error.Diagnose qualified as Diagnose
import Focus.Cli (InputLocation (..), Options (..), OutputLocation (..), UseColour (..), optionsP)
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
import UnliftIO qualified as IO
import UnliftIO.IO qualified as UnliftIO

type CliM = ReaderT Options IO

run :: IO ()
run = do
  opts@Options {command, input, output, chunkSize} <-
    Opts.execParser $
      ( Opts.info
          (optionsP <**> Opts.helper)
          ( Opts.fullDesc
              <> Opts.progDesc "Focus - cli utility for hacking and slashing data"
          )
      )
  withHandles input output \inputHandle outputHandle -> flip runReaderT opts do
    result <- case command of
      View script -> do
        focus <- getFocus "<selector>" ViewF script
        liftIO . runExceptT . runFocusM $ Exec.runView focus chunkSize inputHandle outputHandle
      Modify script m -> do
        focus <- getFocus "<selector>" ModifyF script
        modifier <- getFocus "<modifier>" ModifyF m
        liftIO . runExceptT . runFocusM $ Exec.runModify focus modifier chunkSize inputHandle outputHandle
      Set script val -> do
        focus <- getFocus "<selector>" ModifyF script
        liftIO . runExceptT . runFocusM $ Exec.runSet focus chunkSize inputHandle outputHandle val
    case result of
      Left err -> do
        case err of
          ShellError msg -> do
            liftIO $ TextIO.hPutStrLn UnliftIO.stderr msg
            liftIO $ System.exitFailure
      Right () -> pure ()
  where
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

    withHandles :: InputLocation -> OutputLocation -> (IO.Handle -> IO.Handle -> IO r) -> IO r
    withHandles input output action = do
      let f = case input of
            StdIn -> \go -> go IO.stdin
            InputFile path ->
              \go -> IO.openFile path IO.ReadMode >>= go
      case output of
        StdOut -> f (\inputHandle -> action inputHandle IO.stdout)
        OutputFile path -> f (\inputHandle -> IO.withFile path IO.WriteMode \outputHandle -> action inputHandle outputHandle)

    failWithReport :: Text -> Text -> Diagnose.Report Text -> CliM a
    failWithReport srcName src report = do
      let diagnostic =
            ( Diagnose.addFile mempty (Text.unpack srcName) (Text.unpack src)
                <> (Diagnose.addReport mempty report)
            )
      failWithDiagnostic diagnostic
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
            Left errReport -> failWithReport srcName script errReport
            Right (SomeTypedSelector typedSelector) -> do
              pure $ compileSelector cmdF typedSelector
