{-# LANGUAGE ApplicativeDo #-}

module Focus.Cli
  ( optionsP,
    Options (..),
    InputLocation (..),
    OutputLocation (..),
  )
where

import Data.Function
import Data.Functor
import Data.Text (Text)
import Focus.Command (Command (..))
import Options.Applicative hiding (command)
import Options.Applicative qualified as Opt

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
    command :: Command
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
  command <-
    subparser
      ( Opt.command "view" (info viewP (progDesc "View the focus"))
          <> Opt.command "modify" (info overP (progDesc "Modify the focused field"))
          <> Opt.command "set" (info setP (progDesc "Set the focus"))
      )
  pure Options {input, output, command}

viewP :: Parser Command
viewP = do
  script <- scriptP
  pure $ View script

overP :: Parser Command
overP = do
  script <- scriptP
  modifier <-
    strArgument
      ( metavar "MODIFIER"
          <> help "Modifier to apply"
      )
  pure $ Modify script modifier

setP :: Parser Command
setP = do
  script <- scriptP
  val <-
    strArgument
      ( metavar "VALUE"
          <> help "Value to set"
      )
  pure $ Set script val

scriptP :: Parser Text
scriptP =
  strArgument
    ( metavar "COMMAND"
        <> help "Command to run"
    )
