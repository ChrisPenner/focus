{-# LANGUAGE ApplicativeDo #-}

module Focus.Cli
  ( optionsP,
    Options (..),
  )
where

import Data.Function
import Data.Functor
import Data.Text (Text)
import Options.Applicative

data InputLocation
  = StdIn
  | InputFile FilePath

data OutputLocation
  = StdOut
  | OutputFile FilePath

data Options
  = Options
  { input :: InputLocation,
    output :: OutputLocation,
    script :: Text
  }

optionsP :: Parser Options
optionsP = do
  input <-
    strOption
      ( long "input"
          <> short 'i'
          <> metavar "INPUT-FILE"
          <> help "File to use for input. Defaults to stdin if unspecified"
      )
      & optional
      <&> maybe StdIn InputFile
  output <-
    strOption
      ( long "output"
          <> short 'o'
          <> metavar "OUTPUT-FILE"
          <> help "File to use for output. Defaults to stdout if unspecified"
      )
      & optional
      <&> maybe StdOut OutputFile
  script <-
    strArgument
      ( metavar "COMMAND"
          <> help "Command to run"
      )
  pure Options {input, output, script}
