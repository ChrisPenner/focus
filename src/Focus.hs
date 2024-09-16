module Focus (run) where

import Control.Applicative
import Data.Text (Text)
import Data.Text qualified as Text
import Error.Diagnose qualified as Diagnose
import Focus.AST (typecheckSelector, untagSelector)
import Focus.Cli (InputLocation (..), Options (..), OutputLocation (..), optionsP)
import Focus.Command (Command (..), CommandF (..))
import Focus.Compile (Focus, compileSelector)
import Focus.Debug (debugM)
import Focus.Exec qualified as Exec
import Focus.Parser (parseScript)
import Options.Applicative qualified as Opts
import System.Exit qualified as System
import UnliftIO qualified as IO
import UnliftIO.IO qualified as System

run :: IO ()
run = do
  Options {command, input, output} <-
    Opts.execParser $
      ( Opts.info
          (optionsP <**> Opts.helper)
          ( Opts.fullDesc
              <> Opts.progDesc "Focus - cli utility for hacking and slashing data"
          )
      )
  withHandles input output \inputHandle outputHandle -> do
    case command of
      View script -> do
        focus <- getFocus "<selector>" ViewF script
        Exec.runView focus inputHandle outputHandle
      Modify script m -> do
        focus <- getFocus "<selector>" ModifyF script
        modifier <- getFocus "<modifier>" ModifyF m
        Exec.runModify focus modifier inputHandle outputHandle
      Set script val -> do
        focus <- getFocus "<selector>" ModifyF script
        Exec.runSet focus inputHandle outputHandle val
  where
    failWithDiagnostic :: Diagnose.Diagnostic Text -> IO a
    failWithDiagnostic diagnostic = do
      Diagnose.printDiagnostic System.stderr Diagnose.WithUnicode (Diagnose.TabSize 2) Diagnose.defaultStyle diagnostic
      System.exitFailure

    withHandles :: InputLocation -> OutputLocation -> (IO.Handle -> IO.Handle -> IO r) -> IO r
    withHandles input output action = do
      let f = case input of
            StdIn -> \go -> go IO.stdin
            InputFile path ->
              \go -> IO.openFile path IO.ReadMode >>= go
      case output of
        StdOut -> f (\inputHandle -> action inputHandle IO.stdout)
        OutputFile path -> f (\inputHandle -> IO.withFile path IO.WriteMode \outputHandle -> action inputHandle outputHandle)
    getFocus :: Text -> CommandF cmd -> Text -> IO (Focus cmd IO)
    getFocus srcName cmdF script = do
      case parseScript srcName script of
        Left errDiagnostic -> do
          Diagnose.printDiagnostic System.stderr Diagnose.WithUnicode (Diagnose.TabSize 2) Diagnose.defaultStyle errDiagnostic
          System.exitFailure
        Right ast -> do
          debugM "Selector" ast
          case typecheckSelector ast of
            Left errReport -> do
              let diagnostic =
                    ( Diagnose.addFile mempty (Text.unpack srcName) (Text.unpack script)
                        <> (Diagnose.addReport mempty errReport)
                    )
              failWithDiagnostic diagnostic
            Right _ -> do
              pure $ compileSelector cmdF (untagSelector ast)
