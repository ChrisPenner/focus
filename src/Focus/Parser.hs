{-# LANGUAGE TupleSections #-}

module Focus.Parser (parseScript) where

import Control.Monad
import Data.Bifunctor (Bifunctor (..))
import Data.Function
import Data.Functor (($>))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Error.Diagnose (Diagnostic)
import Error.Diagnose qualified as D
import Error.Diagnose qualified as Diagnose
import Error.Diagnose.Compat.Megaparsec
import Focus.Prelude
import Focus.Untyped
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Regex.PCRE.Heavy qualified as Regex
import Text.Regex.PCRE.Light qualified as PCRE.Light

data CustomError
  = BadRegex Text
  deriving stock (Show, Eq, Ord)

instance M.ShowErrorComponent CustomError where
  showErrorComponent = \case
    BadRegex s -> "Invalid Regex: " <> Text.unpack s

instance HasHints CustomError a where
  hints e = case e of
    BadRegex _ -> []

type P = M.Parsec CustomError String

withPos :: P (Pos -> a) -> P a
withPos p = do
  M.SourcePos {M.sourceName, M.sourceLine = startLine, M.sourceColumn = startCol} <- M.getSourcePos
  f <- p
  end <- M.getSourcePos
  let pos = Diagnose.Position (M.unPos startLine, M.unPos startCol) (M.unPos (M.sourceLine end), M.unPos (M.sourceColumn end)) sourceName
  pure $ f pos

parseScript :: Text -> Text -> Either (Diagnostic Text) TaggedSelector
parseScript srcName src =
  let strSource = Text.unpack src
      strSourceName = Text.unpack srcName
   in M.parse (M.space *> scriptP <* M.eof) strSourceName strSource
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
    lexeme $ M.between (M.char '"') (M.char '"') $ do
      Text.pack <$> M.many (escaped <|> M.anySingleBut '"')
  where
    escaped = M.char '\\' >> M.anySingle

regexP :: P TaggedSelector
regexP = do
  regexLiteralP <&> \(pos, re, bindings) -> Regex pos re bindings

regexLiteralP :: P (Pos, PCRE.Light.Regex, BindingDeclarations)
regexLiteralP =
  withPos $ M.label "regex" $ do
    pat <- lexeme $ M.between (M.char '/') (M.char '/') $ do
      Text.pack <$> many (escaped <|> M.anySingleBut '/')
    case Regex.compileM (Text.encodeUtf8 pat) [] of
      Left err -> M.customFailure $ BadRegex (Text.pack err)
      Right re -> pure $ \pos ->
        let bindingDeclarations =
              (PCRE.Light.captureNames re)
                & fmap (Text.decodeUtf8 . fst)
                & fmap (,(pos, TextType))
                & M.fromList
         in (pos, re, bindingDeclarations)
  where
    escaped = M.string "\\/" $> '/'

reGroupsP :: P (D.Position -> TaggedSelector)
reGroupsP = do
  regexLiteralP <&> \(_pos, re, bindings) -> \pos -> RegexGroups pos re bindings

listOfP :: P TaggedSelector
listOfP = withPos do
  M.between (lexeme $ M.char '[') (lexeme $ M.char ']') $ do
    flip ListOf <$> selectorsP

shellP :: P TaggedSelector
shellP = withPos do
  shellMode <-
    optional (M.char '-') >>= \case
      Just _ -> pure NullStdin
      Nothing -> pure Normal
  M.between (lexeme $ M.char '{') (lexeme $ M.char '}') $ do
    script <- many do
      M.choice
        [ Left <$> withPos ((,) <$> bindingP),
          Right . Text.pack
            <$> some (escaped <|> M.noneOf ("%}" :: String))
        ]
    pure \pos -> Shell pos (BindingString script) shellMode
  where
    -- Escape bindings
    escaped = M.string "\\%" $> '%'

bindingP :: P BindingName
bindingP = M.try do
  M.between (lexeme (M.string "%{")) (M.char '}') $ do
    BindingName . Text.pack <$> lexeme (M.some M.alphaNumChar)

groupedP :: P TaggedSelector
groupedP = do
  shellP <|> listOfP <|> regexP <|> M.between (lexeme (M.char '(')) (lexeme (M.char ')')) selectorsP <|> simpleSelectorP

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
            M.string "filter",
            M.string "...",
            M.string "groups",
            M.string "takeEnd",
            M.string "dropEnd",
            M.string "take",
            M.string "drop",
            M.string "contains"
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
    "groups" -> do
      reGroupsP
    "filter" -> do
      flip Filter <$> groupedP
    "..." -> do
      pure Splat
    "take" -> do
      n <- lexeme L.decimal
      selector <- groupedP
      pure $ \pos -> Take pos n selector
    "drop" -> do
      n <- lexeme L.decimal
      selector <- groupedP
      pure $ \pos -> Drop pos n selector
    "takeEnd" -> do
      n <- lexeme L.decimal
      selector <- groupedP
      pure $ \pos -> TakeEnd pos n selector
    "dropEnd" -> do
      n <- lexeme L.decimal
      selector <- groupedP
      pure $ \pos -> DropEnd pos n selector
    "contains" -> do
      str <- strP
      pure $ \pos -> Contains pos str
    _ -> error "impossible"
