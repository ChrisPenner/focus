{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Focus.Parser (parseScript) where

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

parseScript :: Text -> Either (Diagnostic Text) AST
parseScript src =
  let strSource = Text.unpack src
      srcName = "<selector>"
   in parse scriptP srcName strSource
        & first
          ( \bundle ->
              bundle
                & errorDiagnosticFromBundle Nothing "Invalid selector" Nothing
                & \d -> Diagnose.addFile d srcName strSource
          )

scriptP :: P AST
scriptP = do
  Compose <$> selectorsP

selectorsP :: P (NE.NonEmpty Selector)
selectorsP = NE.fromList <$> M.sepBy1 selectorP separatorP

lexeme :: P a -> P a
lexeme = L.lexeme M.space

separatorP :: P ()
separatorP = do
  void $ lexeme (M.string "|")

-- | Parse a string literal, handling escape sequences
strP :: P Text
strP = M.label "string" $ do
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

listP :: P Selector
listP = do
  between (lexeme $ M.char '[') (lexeme $ M.char ']') $ do
    ListOf . Compose <$> selectorsP

shellP :: P Selector
shellP = do
  between (lexeme $ M.char '{') (lexeme $ M.char '}') $ do
    Shell . Text.pack <$> many (escaped <|> M.anySingleBut '}')
  where
    escaped = M.char '\\' >> M.anySingle

atP :: P Selector
atP = do
  n <- lexeme L.decimal
  pure $ At n

selectorP :: P Selector
selectorP = shellP <|> listP <|> simpleSelectorP

simpleSelectorP :: P Selector
simpleSelectorP = do
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
      pure $ SplitFields delim
    "words" -> pure SplitWords
    "lines" -> pure SplitLines
    "regex" -> do
      pat <- strP <|> regexP
      pure $ Regex pat
    "at" -> atP
    _ -> error "impossible"
