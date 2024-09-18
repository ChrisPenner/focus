module Focus.Parser (parseScript) where

import Control.Monad
import Data.Bifunctor (Bifunctor (..))
import Data.Function
import Data.Functor (($>))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Error.Diagnose (Diagnostic)
import Error.Diagnose qualified as D
import Error.Diagnose qualified as Diagnose
import Error.Diagnose.Compat.Megaparsec
import Focus.AST
import Focus.Prelude ((<&>))
import Text.Megaparsec
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Regex.PCRE.Heavy qualified as Regex

data CustomError
  = BadRegex Text
  deriving stock (Show, Eq, Ord)

instance ShowErrorComponent CustomError where
  showErrorComponent = \case
    BadRegex s -> "Invalid Regex: " <> Text.unpack s

instance HasHints CustomError a where
  hints e = case e of
    BadRegex _ -> []

type P = Parsec CustomError String

withPos :: P (D.Position -> TaggedSelector) -> P TaggedSelector
withPos p = do
  SourcePos {sourceName, sourceLine = startLine, sourceColumn = startCol} <- M.getSourcePos
  f <- p
  end <- M.getSourcePos
  let pos = Diagnose.Position (M.unPos startLine, M.unPos startCol) (M.unPos (M.sourceLine end), M.unPos (M.sourceColumn end)) sourceName
  pure $ f pos

parseScript :: Text -> Text -> Either (Diagnostic Text) TaggedSelector
parseScript srcName src =
  let strSource = Text.unpack src
      strSourceName = Text.unpack srcName
   in parse (M.space *> scriptP <* M.eof) strSourceName strSource
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
  M.sepBy1 selectorP separatorP
    <&> \r pos -> Compose pos . NE.fromList $ r

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

regexP :: P TaggedSelector
regexP =
  withPos $ M.label "regex" $ do
    pat <- lexeme $ between (M.char '/') (M.char '/') $ do
      Text.pack <$> many (escaped <|> M.anySingleBut '/')
    case Regex.compileM (Text.encodeUtf8 pat) [] of
      Left err -> M.customFailure $ BadRegex (Text.pack err)
      Right re -> pure $ \pos -> Regex pos re
  where
    escaped = M.string "\\/" $> '/'

listOfP :: P TaggedSelector
listOfP = withPos do
  between (lexeme $ M.char '[') (lexeme $ M.char ']') $ do
    flip ListOf <$> selectorsP

shellP :: P TaggedSelector
shellP = withPos do
  between (lexeme $ M.char '{') (lexeme $ M.char '}') $ do
    flip Shell . Text.pack <$> many (escaped <|> M.anySingleBut '}')
  where
    escaped = M.char '\\' >> M.anySingle

groupedP :: P TaggedSelector
groupedP = do
  shellP <|> listOfP <|> regexP <|> between (lexeme (M.char '(')) (lexeme (M.char ')')) selectorsP

selectorP :: P TaggedSelector
selectorP = shellP <|> listOfP <|> regexP <|> simpleSelectorP

simpleSelectorP :: P TaggedSelector
simpleSelectorP = withPos do
  name <-
    lexeme
      ( M.choice
          [ M.string "splitOn",
            M.string "words",
            M.string "lines",
            M.string "at",
            M.string "matches",
            M.string "filterBy",
            M.string "..."
          ]
      )
  case name of
    "splitOn" -> do
      delim <- strP
      pure $ flip SplitFields delim
    "words" -> pure SplitWords
    "lines" -> pure SplitLines
    "at" -> do
      n <- lexeme L.decimal
      pure $ flip At n
    "matches" -> do
      pure RegexMatches
    "groups" -> do
      pure RegexGroups
    "filterBy" -> do
      flip FilterBy <$> groupedP
    "..." -> do
      pure Splat
    _ -> error "impossible"
