{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TupleSections #-}

module Focus.Parser (parseSelector, parseAction) where

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
import Focus.Debug qualified as Debug
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

parseSelector :: (forall x. (Show x) => Show (expr x)) => Text -> Text -> Either (Diagnostic Text) (Selector expr Pos)
parseSelector srcName src = parseThing (scriptP noExprP) "Invalid selector" srcName src
  where
    noExprP = actionP *> fail "Expressions are not valid where a selector was expected."

parseAction :: Text -> Text -> Either (Diagnostic Text) TaggedAction
parseAction srcName src = parseThing actionP "Invalid expression" srcName src

parseThing :: (Show a) => P a -> Text -> Text -> Text -> Either (Diagnostic Text) a
parseThing parser err srcName src = do
  let strSource = Text.unpack src
      strSourceName = Text.unpack srcName
  result <-
    M.parse (M.space *> parser <* M.eof) strSourceName strSource
      & first
        ( \bundle ->
            bundle
              & errorDiagnosticFromBundle Nothing err Nothing
              & \d -> Diagnose.addFile d strSourceName strSource
        )
  Debug.debugM srcName $ "Parsed: " <> show result
  pure result

scriptP :: (P (expr Pos)) -> P (Selector expr Pos)
scriptP expr = selectorsP expr

selectorsP :: P (expr Pos) -> P (Selector expr Pos)
selectorsP expr = withPos do
  M.sepBy1 (selectorP expr) separatorP
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

listOfP :: P (expr Pos) -> P (Selector expr Pos)
listOfP expr = withPos do
  M.between (lexeme $ M.char '[') (lexeme $ M.char ']') $ do
    flip ListOf <$> selectorsP expr

shellP :: P (Selector expr Pos)
shellP = withPos do
  shellMode <-
    optional (M.char '-') >>= \case
      Just _ -> pure NullStdin
      Nothing -> pure Normal
  script <- bindingStringP '{' '}'
  pure $ \pos -> Shell pos script shellMode

bindingStringP :: Char -> Char -> P BindingString
bindingStringP begin end = M.between (lexeme $ M.char begin) (lexeme $ M.char end) $ do
  BindingString <$> many do
    M.choice
      [ Left <$> withPos ((,) <$> bareBindingP),
        Right . Text.pack
          <$> some (escaped <|> M.noneOf (['%', '}', end] :: String))
      ]
  where
    -- Escape bindings
    escaped = M.string "\\%" $> '%'

bareBindingP :: P BindingName
bareBindingP = M.try do
  M.between (lexeme (M.string "%{")) (M.char '}') $
    do
      (BindingName . Text.pack <$> lexeme (M.some M.alphaNumChar))
      <|> (M.string "." $> InputBinding)

groupedP :: P (expr Pos) -> P (Selector expr Pos)
groupedP expr = do
  shellP <|> listOfP expr <|> regexP <|> bracketedP (selectorsP expr) <|> bracketedP (simpleSelectorP expr)

bracketedP :: P a -> P a
bracketedP p = M.between (lexeme (M.char '(')) (lexeme (M.char ')')) p

mayBracketedP :: P a -> P a
mayBracketedP p = bracketedP p <|> p

selectorP :: P (expr Pos) -> P (Selector expr Pos)
selectorP expr = shellP <|> listOfP expr <|> regexP <|> groupedP expr <|> simpleSelectorP expr <|> evalP expr

evalP :: P (expr Pos) -> P (Selector expr Pos)
evalP expr = withPos do flip Action <$> expr

simpleSelectorP :: P (expr Pos) -> P (Selector expr Pos)
simpleSelectorP expr = withPos do
  caseMatchP
    [ ( "splitOn",
        do
          delim <- strP
          pure $ flip SplitFields delim
      ),
      ( "words",
        pure SplitWords
      ),
      ( "lines",
        pure SplitLines
      ),
      ( "at",
        do
          n <- lexeme L.decimal
          pure $ flip At n
      ),
      ( "groups",
        do
          reGroupsP
      ),
      ( "filter",
        do
          flip Filter <$> groupedP expr
      ),
      ( "...",
        do
          pure Splat
      ),
      ( "takeEnd",
        do
          n <- lexeme L.decimal
          selector <- groupedP expr
          pure $ \pos -> TakeEnd pos n selector
      ),
      ( "dropEnd",
        do
          n <- lexeme L.decimal
          selector <- groupedP expr
          pure $ \pos -> DropEnd pos n selector
      ),
      ( "take",
        do
          n <- lexeme L.decimal
          selector <- groupedP expr
          pure $ \pos -> Take pos n selector
      ),
      ( "drop",
        do
          n <- lexeme L.decimal
          selector <- groupedP expr
          pure $ \pos -> Drop pos n selector
      ),
      ( "contains",
        do
          str <- strP
          pure $ \pos -> Contains pos str
      ),
      ( "not",
        do
          selector <- selectorP expr
          pure $ \pos -> Not pos selector
      )
    ]

caseMatchP :: [(String, P a)] -> P a
caseMatchP theCases =
  do
    theCases <&> \(name, p) -> lexeme (M.string name) *> p
    & M.choice

numberP :: P NumberT
numberP = lexeme $ do
  M.choice
    [ fmap DoubleNumber . M.try $ L.signed (pure ()) L.float,
      fmap IntNumber . M.try $ L.signed (pure ()) L.decimal
    ]

actionP :: P (Selector Expr Pos)
actionP = selectorsP basicExprP

basicExprP :: P TaggedExpr
basicExprP = mayBracketedP do
  M.choice
    [ exprLiteralP,
      simpleExprP
    ]

simpleExprP :: P TaggedExpr
simpleExprP = withPos $ do
  caseMatchP $
    [ ( "concat",
        do
          expr <- actionP
          pure $ flip StrConcat expr
      ),
      ( "intersperse",
        do
          a <- lexeme actionP
          rest <- some (lexeme actionP)
          pure $ \pos -> Intersperse pos (a NE.:| rest)
      )
    ]

exprLiteralP :: P (Expr Pos)
exprLiteralP = withPos do
  M.choice
    [ flip Number <$> numberP,
      flip Binding <$> bareBindingP,
      flip Str <$> bindingStringP '"' '"'
    ]
