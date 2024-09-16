{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Focus.Parser (parseScript) where

import Control.Comonad.Cofree qualified as CF
import Control.Monad
import Data.Bifunctor (Bifunctor (..))
import Data.Function
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void
import Error.Diagnose (Diagnostic)
import Error.Diagnose qualified as Diagnose
import Error.Diagnose.Compat.Megaparsec
import Focus.AST
import Text.Megaparsec
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M
import Text.Megaparsec.Char.Lexer qualified as L

type P = Parsec Void String

instance HasHints Void a where
  hints e = case e of {}

withPos :: P (SelectorF TaggedSelector) -> P TaggedSelector
withPos p = do
  SourcePos {sourceName, sourceLine = startLine, sourceColumn = startCol} <- M.getSourcePos
  s <- p
  end <- M.getSourcePos
  let pos = Diagnose.Position (M.unPos startLine, M.unPos startCol) (M.unPos (M.sourceLine end), M.unPos (M.sourceColumn end)) sourceName
  pure $ pos CF.:< s

parseScript :: Text -> Text -> Either (Diagnostic Text) TaggedSelector
parseScript srcName src =
  let strSource = Text.unpack src
      strSourceName = Text.unpack srcName
   in parse (scriptP <* M.eof) strSourceName strSource
        & first
          ( \bundle ->
              bundle
                & errorDiagnosticFromBundle Nothing "Invalid selector" Nothing
                & \d -> Diagnose.addFile d strSourceName strSource
          )

scriptP :: P TaggedSelector
scriptP = selectorsP

selectorsP :: P TaggedSelector
selectorsP = withPos do
  ComposeF . NE.fromList <$> M.sepBy1 selectorP separatorP

lexeme :: P a -> P a
lexeme = L.lexeme M.space

separatorP :: P ()
separatorP = do
  void $ lexeme (M.string "|")

-- | Parse a string literal, handling escape sequences
strP :: P Text
strP =
  M.label "string" $ do
    lexeme $ between (M.char '"') (M.char '"') $ do
      Text.pack <$> many (escaped <|> M.anySingleBut '"')
  where
    escaped = M.char '\\' >> M.anySingle

regexP :: P Text
regexP =
  M.label "regex, e.g. /he?(ll)o.+/" $ do
    lexeme $ between (M.char '/') (M.char '/') $ do
      Text.pack <$> many (escaped <|> M.anySingleBut '/')
  where
    escaped = M.char '\\' >> M.anySingle

listP :: P TaggedSelector
listP = withPos do
  between (lexeme $ M.char '[') (lexeme $ M.char ']') $ do
    ListOfF <$> selectorsP

shellP :: P TaggedSelector
shellP = withPos do
  between (lexeme $ M.char '{') (lexeme $ M.char '}') $ do
    ShellF . Text.pack <$> many (escaped <|> M.anySingleBut '}')
  where
    escaped = M.char '\\' >> M.anySingle

selectorP :: P TaggedSelector
selectorP = shellP <|> listP <|> simpleSelectorP

simpleSelectorP :: P TaggedSelector
simpleSelectorP = withPos do
  name <-
    lexeme
      ( M.choice
          [ M.string "splitOn",
            M.string "words",
            M.string "lines",
            M.string "regex",
            M.string "at"
          ]
      )
  case name of
    "splitOn" -> do
      delim <- strP
      pure $ SplitFieldsF delim
    "words" -> pure SplitWordsF
    "lines" -> pure SplitLinesF
    "regex" -> do
      pat <- strP <|> regexP
      pure $ RegexF pat
    "at" -> do
      n <- lexeme L.decimal
      pure $ AtF n
    _ -> error "impossible"
