module Focus (run) where

import Control.Applicative
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as IO
import Focus.Cli (Options (..), optionsP)
import Focus.Parser (parseScript)
import Options.Applicative qualified as Opts
import System.Exit qualified as System
import UnliftIO qualified as IO

run :: IO ()
run = do
  Options {script} <-
    Opts.execParser $
      ( Opts.info
          (optionsP <**> Opts.helper)
          ( Opts.fullDesc
              <> Opts.progDesc "Focus - cli utility for hacking and slashing data"
          )
      )
  case parseScript script of
    Left err -> do
      failWith err
    Right ast -> do
      failWith (Text.pack $ show ast)
  where
    failWith :: Text -> IO a
    failWith msg = do
      IO.hPutStrLn IO.stderr msg
      System.exitFailure
