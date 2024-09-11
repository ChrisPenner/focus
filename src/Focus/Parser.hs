module Focus.Parser (parseScript) where

import Control.Monad
import Data.Bifunctor (Bifunctor (..))
import Data.Function
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void
import Focus.AST
import Text.Megaparsec
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M
import Text.Megaparsec.Char.Lexer qualified as L

type P = Parsec Void String

parseScript :: Text -> Either Text AST
parseScript src =
  parse scriptP "<script>" (Text.unpack src)
    & first (Text.pack . errorBundlePretty)

scriptP :: P AST
scriptP = do
  selectors <- NE.fromList <$> M.sepBy1 selectorP separatorP
  pure $ Compose selectors

lexeme :: P a -> P a
lexeme = L.lexeme M.space

separatorP :: P ()
separatorP = do
  void $ lexeme (M.string "|")

-- | Parse a string literal, handling escape sequences
strP :: P Text
strP =
  lexeme $ between (M.char '"') (M.char '"') $ do
    Text.pack <$> many (escaped <|> M.anySingleBut '"')
  where
    escaped = M.char '\\' >> M.anySingle

regexP :: P Text
regexP =
  lexeme $ between (M.char '/') (M.char '/') $ do
    Text.pack <$> many (escaped <|> M.anySingleBut '/')
  where
    escaped = M.char '\\' >> M.anySingle

selectorP :: P Selector
selectorP = do
  name <-
    lexeme
      ( M.choice
          [ M.string "splitOn",
            M.string "words",
            M.string "lines",
            M.string "regex"
          ]
      )
  case name of
    "splitOn" -> do
      delim <- strP
      pure $ SplitFields delim
    "words" -> pure SplitWords
    "lines" -> pure SplitLines
    "regex" -> do
      pat <- strP <|> regexP
      pure $ Regex pat
    _ -> error "impossible"
