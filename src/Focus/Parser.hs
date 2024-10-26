{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TupleSections #-}

module Focus.Parser (parseSelector) where

import Data.Bifunctor (Bifunctor (..))
import Data.Function
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Map qualified as Map
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
  | ActionInSelector
  deriving stock (Show, Eq, Ord)

instance M.ShowErrorComponent CustomError where
  showErrorComponent = \case
    BadRegex s -> "Invalid Regex: " <> Text.unpack s
    ActionInSelector -> "Expressions are not valid where a selector was expected."

instance HasHints CustomError Text where
  hints e = case e of
    BadRegex _ -> []
    ActionInSelector -> [D.Note "All components of a selector must be *reversible*"]

type P = M.Parsec CustomError String

withPos :: P (Pos -> a) -> P a
withPos p = do
  M.SourcePos {M.sourceName, M.sourceLine = startLine, M.sourceColumn = startCol} <- M.getSourcePos
  f <- p
  end <- M.getSourcePos
  let pos = Diagnose.Position (M.unPos startLine, M.unPos startCol) (M.unPos (M.sourceLine end), M.unPos (M.sourceColumn end)) sourceName
  pure $ f pos

parseSelector :: Text -> Text -> Either (Diagnostic Text) (Selector Pos)
parseSelector srcName src = parseThing selectorsP "Invalid selector" srcName src

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

data Separator = Pipe | PipeModify

selectorsP :: P (Selector Pos)
selectorsP = withPos do
  sel <- selectorP
  sep <- optional separatorP
  case sep of
    Nothing -> pure $ const sel
    Just PipeModify -> do
      rest <- selectorsP
      pure $ \pos -> Modify pos sel rest
    Just Pipe -> do
      rest <- selectorsP
      pure $ \pos -> Compose pos (sel NE.:| [rest])
  where
    separatorP :: P Separator
    separatorP = lexeme do
      M.choice
        [ M.string "|=" $> PipeModify,
          M.string "|" $> Pipe
        ]

lexeme :: P a -> P a
lexeme = L.lexeme M.space

-- | Parse a string literal, handling escape sequences
strP :: P Text
strP =
  M.label "string" $ do
    lexeme $ M.between (M.char '"') (M.char '"') $ do
      Text.pack <$> M.many (escaped <|> M.anySingleBut '"')
  where
    escaped = M.char '\\' >> M.anySingle

regexP :: P (Selector Pos)
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

reGroupsP :: P (D.Position -> (Selector Pos))
reGroupsP = do
  regexLiteralP <&> \(_pos, re, bindings) -> \pos -> RegexGroups pos re bindings

listOfP :: P (Selector Pos)
listOfP = withPos do
  M.between (lexeme $ M.char '[') (lexeme $ M.char ']') $ do
    flip ListOf <$> selectorsP

shellP :: P (Expr Pos)
shellP = withPos do
  shellMode <-
    optional (M.try $ M.char '-' *> M.lookAhead (M.char '{')) >>= \case
      Just _ -> pure NullStdin
      Nothing -> pure Normal
  script <- templateStringP '{' '}'
  pure $ \pos -> Shell pos script shellMode

shellActionP :: P (Selector Pos)
shellActionP = withPos do
  flip Action <$> shellP

recordP :: P (Selector Pos)
recordP = withPos do
  _ <- (M.char ':' *> M.lookAhead (M.char '{'))
  fields <- Map.fromList <$> M.between (lexeme $ M.char '{') (lexeme $ M.char '}') (M.sepBy field (lexeme $ M.char '#'))
  pure $ \pos -> Record pos fields
  where
    field = do
      key <- lexeme bindingName
      _ <- lexeme $ M.char ':'
      value <- selectorP
      pure (key, value)

templateStringP :: Char -> Char -> P (TemplateString D.Position)
templateStringP begin end = M.between (lexeme $ M.char begin) (lexeme $ M.char end) $ do
  TemplateString <$> many do
    M.choice
      [ Left <$> selExprP,
        Right . Text.pack
          <$> some (escaped <|> M.noneOf (['%', '}', end] :: String))
      ]
  where
    selExprP =
      M.try (M.char '%' *> M.between (lexeme (M.char '{')) (lexeme (M.char '}')) selectorsP)
        <|> (M.try $ withPos ((\b pos -> Action pos $ Binding pos b) <$> bareBindingP))
    -- Escape bindings
    escaped = M.string "\\%" $> '%'

patternStringP :: Char -> Char -> P (Selector Pos)
patternStringP begin end = withPos $ M.between (lexeme $ M.char begin) (lexeme $ M.char end) $ do
  patPieces <- many . withPos $ do
    M.choice
      [ flip PatternBinding <$> bareBindingNameP,
        -- flip PatternSelector <$> selExprP,
        flip PatternText . Text.pack
          <$> some (escaped <|> M.noneOf (['%', end] :: String))
      ]
  Debug.debugM "PatternString" $ "Pattern pieces: " <> show patPieces
  let (bindingDecls, regexStr) =
        patPieces & foldMap \case
          PatternText _pos t -> (mempty, Regex.escape t)
          PatternBinding pos t -> (M.singleton t (pos, TextType), "(?<" <> t <> ">.+?|.*)")
  case Regex.compileM (Text.encodeUtf8 regexStr) [] of
    Left err -> M.customFailure $ BadRegex (Text.pack err)
    Right re -> do
      Debug.debugM "Regex" $ re
      pure $ \pos -> Regex pos re bindingDecls
  where
    -- selExprP =
    --   M.try (M.char '%' *> M.between (lexeme (M.char '{')) (lexeme (M.char '}')) selectorsP)
    --     <|> (M.try $ withPos ((\b pos -> Action pos $ Binding pos b) <$> bareBindingP))
    -- Escape bindings
    escaped = M.string "\\%" $> '%'

    bareBindingNameP :: P Text
    bareBindingNameP = do
      _ <- M.char '%'
      bindingName

bareBindingP :: P BindingName
bareBindingP = do
  _ <- M.char '%'
  ((BindingName <$> bindingName) <|> (M.string "." $> InputBinding))

bindingName :: P Text
bindingName = do
  Text.pack <$> (M.some M.alphaNumChar)

groupedP :: P (Selector Pos)
groupedP = do
  castP <|> shellActionP <|> listOfP <|> regexP <|> bracketedP selectorsP <|> bracketedP simpleSelectorP

castP :: P (Selector Pos)
castP = withPos do
  _ <- (lexeme (M.char '!'))
  selector <- groupedP
  pure $ \pos -> Compose pos (selector NE.:| [Cast pos])

bracketedP :: P a -> P a
bracketedP p = M.between (lexeme (M.char '(')) (lexeme (M.char ')')) p

mayBracketedP :: P a -> P a
mayBracketedP p = bracketedP p <|> p

selectorP :: P (Selector Pos)
selectorP = withPos do
  l <- sp
  builder <- optional (lexeme ((M.char ',' $> Comma) <|> mathBinOp))
  case builder of
    Just b -> do
      r <- selectorP <|> sp
      pure $ \pos -> Action pos (b pos l r)
    Nothing -> pure $ const l
  where
    mathBinOp :: P (Pos -> Selector Pos -> Selector Pos -> Expr Pos)
    mathBinOp = do
      op <-
        M.choice
          [ M.char '+' $> Plus,
            M.char '-' $> Minus,
            M.char '*' $> Multiply,
            M.char '^' $> Power,
            -- M.char '/' $> Divide,
            M.char '%' $> Modulo
          ]
      pure (\pos -> MathBinOp pos op)
    sp = recordP <|> shellActionP <|> listOfP <|> regexP <|> groupedP <|> simpleSelectorP <|> evalP

evalP :: P (Selector Pos)
evalP = withPos do flip Action <$> basicExprP

simpleSelectorP :: P (Selector Pos)
simpleSelectorP = withPos do
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
          flip Filter <$> groupedP
      ),
      ( "...",
        do
          pure Splat
      ),
      ( "takeEnd",
        do
          n <- lexeme L.decimal
          selector <- groupedP
          pure $ \pos -> TakeEnd pos n selector
      ),
      ( "dropEnd",
        do
          n <- lexeme L.decimal
          selector <- groupedP
          pure $ \pos -> DropEnd pos n selector
      ),
      ( "take",
        do
          n <- lexeme L.decimal
          selector <- groupedP
          pure $ \pos -> Take pos n selector
      ),
      ( "drop",
        do
          n <- lexeme L.decimal
          selector <- groupedP
          pure $ \pos -> Drop pos n selector
      ),
      ( "contains",
        do
          str <- strP
          pure $ \pos -> Contains pos str
      ),
      ( "not",
        do
          selector <- selectorP
          pure $ \pos -> Not pos selector
      ),
      ( "json",
        pure ParseJSON
      ),
      ( "->",
        do
          binding <- lexeme bindingName
          pure $ \pos -> BindingAssignment pos binding
      ),
      ( "pattern",
        do
          const <$> patternStringP '"' '"'
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

basicExprP :: P TaggedExpr
basicExprP = mayBracketedP do
  M.choice
    [ shellP,
      exprLiteralP,
      simpleExprP
    ]

simpleExprP :: P TaggedExpr
simpleExprP = withPos $ do
  caseMatchP $
    [ ( "concat",
        do
          expr <- selectorsP
          pure $ flip StrConcat expr
      ),
      ( "intersperse",
        do
          a <- lexeme selectorsP
          rest <- some (lexeme selectorsP)
          pure $ \pos -> Intersperse pos (a NE.:| rest)
      ),
      ( "count",
        do
          flip Count <$> selectorP
      )
    ]

exprLiteralP :: P (Expr Pos)
exprLiteralP = withPos do
  M.choice
    [ flip Number <$> numberP,
      flip Binding <$> lexeme bareBindingP,
      flip Str <$> templateStringP '"' '"'
    ]
