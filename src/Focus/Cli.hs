{-# LANGUAGE ApplicativeDo #-}

module Focus.Cli
  ( optionsP,
    Options (..),
    UseColour (..),
    ChunkSize (..),
    InPlace (..),
    ShowWarnings (..),
    Alignment (..),
  )
where

import Data.Function
import Data.Functor
import Focus.Command (Command (..), InputLocation (..), OutputLocation (..), ScriptLocation (..))
import Options.Applicative hiding (action, command)

data ChunkSize
  = LineChunks
  | EntireChunks

data UseColour = Colour | NoColour

data InPlace = InPlace | NotInPlace
  deriving stock (Show, Eq, Ord)

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
  command <- cmdP
  pure Options {output, command, useColour, chunkSize, inPlace, showWarnings, alignMode}

inputFilesP :: Parser [FilePath]
inputFilesP = many $ strArgument (metavar "FILES..." <> help "Input files. If omitted, read from stdin")

cmdP :: Parser Command
cmdP = do
  script <- scriptP
  inputLocs <-
    inputFilesP <&> fmap \case
      "-" -> StdIn
      f -> InputFile f
  pure $ Command script inputLocs

alignModeP :: Parser Alignment
alignModeP = do
  flag
    Unaligned
    Aligned
    ( long "align"
        <> short 'a'
        <> help "Align the input files line by line. Refer to them using %f1, %f2, etc."
    )

scriptP :: Parser ScriptLocation
scriptP =
  lit <|> file
  where
    lit =
      ScriptLiteral
        <$> ( strArgument
                ( metavar "SCRIPT"
                    <> help "Focus script to run"
                )
            )
    file =
      ScriptFile
        <$> ( strOption
                ( long "script-file"
                    <> short 'f'
                    <> metavar "SCRIPT-FILE"
                    <> help "File containing the focus script to run"
                )
            )
