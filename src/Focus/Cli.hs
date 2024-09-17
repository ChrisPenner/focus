{-# LANGUAGE ApplicativeDo #-}

module Focus.Cli
  ( optionsP,
    Options (..),
    InputLocation (..),
    OutputLocation (..),
    UseColour (..),
    ChunkSize (..),
    InPlace (..),
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

data ChunkSize
  = LineChunks
  | EntireChunks

data UseColour = Colour | NoColour

data InPlace = InPlace | NotInPlace

data Options
  = Options
  { output :: OutputLocation,
    inPlace :: InPlace,
    command :: Command,
    useColour :: UseColour,
    chunkSize :: ChunkSize
  }

optionsP :: Parser Options
optionsP = do
  output <-
    strOption
      ( long "output"
          <> short 'o'
          <> metavar "OUTPUT-FILE"
          <> help "File to use for output. Defaults to stdout if unspecified"
      )
      & optional
      <&> maybe StdOut OutputFile
  useColour <-
    flag
      Colour
      NoColour
      ( long "no-color"
          <> help "Disable colored output"
      )
  chunkSize <-
    flag
      LineChunks
      EntireChunks
      ( long "full"
          <> short 'f'
          <> help "Process the entire input at once instead of line-by-line"
      )
  inPlace <-
    flag
      NotInPlace
      InPlace
      ( long "in-place"
          <> short 'i'
          <> help "Modify each input file in place rather than writing to output"
      )
  command <-
    subparser
      ( Opt.command "view" (info viewP (progDesc "View the focus"))
          <> Opt.command "modify" (info overP (progDesc "Modify the focused field"))
          <> Opt.command "set" (info setP (progDesc "Set the focus"))
      )
  pure Options {output, command, useColour, chunkSize, inPlace}

inputFilesP :: Parser [FilePath]
inputFilesP = many $ strArgument (metavar "FILES..." <> help "Input files. If omitted, read from stdin")

viewP :: Parser Command
viewP = do
  script <- scriptP
  inputFiles <- inputFilesP
  pure $ View script inputFiles

overP :: Parser Command
overP = do
  script <- scriptP
  modifier <-
    strArgument
      ( metavar "MODIFIER"
          <> help "Modifier to apply"
      )
  inputFiles <- inputFilesP
  pure $ Modify script modifier inputFiles

setP :: Parser Command
setP = do
  script <- scriptP
  val <-
    strArgument
      ( metavar "VALUE"
          <> help "Value to set"
      )
  inputFiles <- inputFilesP
  pure $ Set script val inputFiles

scriptP :: Parser Text
scriptP =
  strArgument
    ( metavar "COMMAND"
        <> help "Command to run"
    )
