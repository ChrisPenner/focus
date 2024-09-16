module Focus (run) where

import Control.Applicative
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as IO
import Focus.AST (typecheckAST)
import Focus.Cli (InputLocation (..), Options (..), OutputLocation (..), optionsP)
import Focus.Command (Command (..), CommandF (..))
import Focus.Compile (Focus, compileAST)
import Focus.Debug (debugM)
import Focus.Exec qualified as Exec
import Focus.Parser (parseScript)
import Options.Applicative qualified as Opts
import System.Exit qualified as System
import UnliftIO qualified as IO

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
        focus <- getFocus ViewF script
        Exec.runView focus inputHandle outputHandle
      Over script m -> do
        focus <- getFocus OverF script
        modifier <- getFocus OverF m
        Exec.runOver focus modifier inputHandle outputHandle
      Set script val -> do
        focus <- getFocus OverF script
        Exec.runSet focus inputHandle outputHandle val
  where
    failWith :: Text -> IO a
    failWith msg = do
      IO.hPutStrLn IO.stderr msg
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
    getFocus :: CommandF cmd -> Text -> IO (Focus cmd IO)
    getFocus cmdF script = do
      case parseScript script of
        Left err -> do
          failWith err
        Right ast -> do
          debugM "AST" ast
          case typecheckAST ast of
            Left err -> do
              failWith $ "Type error: " <> Text.pack (show err)
            Right _ -> do
              pure $ compileAST cmdF ast
