{-# LANGUAGE TupleSections #-}

module Focus.Parser (parseSelector, parseExpr) where

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

parseSelector :: Text -> Text -> Either (Diagnostic Text) (Selector expr Pos)
parseSelector srcName src = parseThing scriptP "Invalid selector" srcName src

parseExpr :: Text -> Text -> Either (Diagnostic Text) TaggedExpr
parseExpr srcName src = parseThing exprP "Invalid expression" srcName src

parseThing :: P a -> Text -> Text -> Text -> Either (Diagnostic Text) a
parseThing parser err srcName src =
  let strSource = Text.unpack src
      strSourceName = Text.unpack srcName
   in M.parse (M.space *> parser <* M.eof) strSourceName strSource
        & first
          ( \bundle ->
              bundle
                & errorDiagnosticFromBundle Nothing err Nothing
                & \d -> Diagnose.addFile d strSourceName strSource
          )

scriptP :: P (Selector expr Pos)
scriptP = selectorsP

selectorsP :: P (Selector expr Pos)
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

regexP :: P (Selector expr Pos)
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

reGroupsP :: P (D.Position -> (Selector expr Pos))
reGroupsP = do
  regexLiteralP <&> \(_pos, re, bindings) -> \pos -> RegexGroups pos re bindings

listOfP :: P (Selector expr Pos)
listOfP = withPos do
  M.between (lexeme $ M.char '[') (lexeme $ M.char ']') $ do
    flip ListOf <$> selectorsP

shellP :: P (Selector expr Pos)
shellP = withPos do
  shellMode <-
    optional (M.char '-') >>= \case
      Just _ -> pure NullStdin
      Nothing -> pure Normal
  M.between (lexeme $ M.char '{') (lexeme $ M.char '}') $ do
    script <- bindingStringP
    pure $ \pos -> Shell pos script shellMode

bindingStringP :: P BindingString
bindingStringP = do
  BindingString <$> many do
    M.choice
      [ Left <$> withPos ((,) <$> strBindingP),
        Right . Text.pack
          <$> some (escaped <|> M.noneOf ("%}" :: String))
      ]
  where
    -- Escape bindings
    escaped = M.string "\\%" $> '%'

strBindingP :: P BindingName
strBindingP = M.try do
  M.between (lexeme (M.string "%{")) (M.char '}') $
    do
      (BindingName . Text.pack <$> lexeme (M.some M.alphaNumChar))
      <|> (M.string "." $> InputBinding)

groupedP :: P (Selector expr Pos)
groupedP = do
  shellP <|> listOfP <|> regexP <|> bracketedP selectorsP <|> bracketedP simpleSelectorP

bracketedP :: P a -> P a
bracketedP p = M.between (lexeme (M.char '(')) (lexeme (M.char ')')) p

mayBracketedP :: P a -> P a
mayBracketedP p = bracketedP p <|> p

selectorP :: P (Selector expr Pos)
selectorP = shellP <|> listOfP <|> regexP <|> groupedP <|> simpleSelectorP

simpleSelectorP :: P (Selector expr Pos)
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
            M.string "contains",
            M.string "not"
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
    "not" -> do
      selector <- selectorP
      pure $ \pos -> Not pos selector
    _ -> error "impossible"

numberP :: P NumberT
numberP = lexeme $ do
  M.choice
    [ fmap DoubleNumber . M.try $ L.signed (pure ()) L.float,
      fmap IntNumber . M.try $ L.signed (pure ()) L.decimal
    ]

bareBindingP :: P BindingName
bareBindingP =
  lexeme do
    (BindingName . Text.pack <$> lexeme (M.some M.alphaNumChar))
    <|> (M.string "." $> InputBinding)

exprP :: P TaggedExpr
exprP = mayBracketedP $ withPos do
  e <- basicExprP
  pipe <- optional do
    separatorP
    selectorP
  case pipe of
    Just s -> do
      pure $ \pos -> Pipeline pos e s
    Nothing -> do
      pure \_ -> e

basicExprP :: P TaggedExpr
basicExprP = mayBracketedP $ withPos do
  M.choice
    [ flip Number <$> numberP,
      flip Binding
        <$> bareBindingP,
      flip Str <$> bindingStringP
    ]
