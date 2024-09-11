module Focus (run) where

import Control.Applicative
import Data.Text (Text)
import Data.Text.IO qualified as IO
import Focus.Cli (InputLocation (..), Options (..), OutputLocation (..), optionsP)
import Focus.Command (Command (..))
import Focus.Compile (Focus, compileAST)
import Focus.Parser (parseScript)
import Focus.View qualified as View
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
        focus <- getFocus script
        View.runView focus inputHandle outputHandle
      Over {} -> error "Over not implemented"
      Set {} -> error "Set not implemented"
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
    getFocus :: Text -> IO Focus
    getFocus script = do
      case parseScript script of
        Left err -> do
          failWith err
        Right ast -> do
          pure $ compileAST ast
