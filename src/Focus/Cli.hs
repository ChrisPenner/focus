{-# LANGUAGE ApplicativeDo #-}

module Focus.Cli
  ( optionsP,
    Options (..),
    InputLocation (..),
    OutputLocation (..),
    UseColour (..),
    ChunkSize (..),
    InPlace (..),
    ShowWarnings (..),
    Alignment (..),
  )
where

import Data.Function
import Data.Functor
import Data.Text (Text)
import Focus.Command (Command (..))
import Options.Applicative hiding (action, command)

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

data ShowWarnings = ShowWarnings | NoWarnings

data Alignment = Aligned | Unaligned

data Options
  = Options
  { output :: OutputLocation,
    inPlace :: InPlace,
    command :: Command,
    useColour :: UseColour,
    chunkSize :: ChunkSize,
    showWarnings :: ShowWarnings,
    alignMode :: Alignment
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
  showWarnings <-
    flag
      ShowWarnings
      NoWarnings
      ( long "silent"
          <> short 's'
          <> help "Suppress warnings"
      )

  alignMode <- alignModeP
  command <- overP
  pure Options {output, command, useColour, chunkSize, inPlace, showWarnings, alignMode}

inputFilesP :: Parser [FilePath]
inputFilesP = many $ strArgument (metavar "FILES..." <> help "Input files. If omitted, read from stdin")

overP :: Parser Command
overP = do
  script <- scriptP
  inputFiles <- inputFilesP
  pure $ Modify script inputFiles

alignModeP :: Parser Alignment
alignModeP = do
  flag
    Unaligned
    Aligned
    ( long "align"
        <> short 'a'
        <> help "Align the input files line by line. Refer to them using %f1, %f2, etc."
    )

scriptP :: Parser Text
scriptP =
  strArgument
    ( metavar "SCRIPT"
        <> help "Focus script to run"
    )
