{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Focus.Untyped
  ( Selector (..),
    TaggedSelector,
    ShellMode (..),
    BindingName (..),
    BindingSymbol (..),
    TemplateString (..),
    PatternElem (..),
    Pattern (..),
    Bindings,
    BindingDeclarations,
    Chunk (..),
    ChunkType (..),
    Pos,
    _TextChunk,
    _ListChunk,
    _NumberChunk,
    _RegexMatchChunk,
    Expr (..),
    TaggedExpr,
    NumberT (..),
    VoidF,
    absurdF,
    renderChunk,
    MathBinOp (..),
    renderNumber,
  )
where

import Control.Lens hiding (Empty, Reversed)
import Control.Lens.Regex.Text qualified as Re
import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.Bitraversable
import Data.ByteString.Lazy.Char8 qualified as BSC
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Error.Diagnose qualified as D
import Focus.Tagged (Tagged (..))
import Text.Regex.PCRE.Heavy (Regex)

type Pos = D.Position

data VoidF p
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

absurdF :: VoidF p -> b
absurdF = \case {}

type TaggedSelector = Selector Pos

-- | A non-special binding symbol, i.e. not '%.', just a regular name like 'foo'
newtype BindingSymbol = BindingSymbol {bsText :: Text}
  deriving stock (Show, Eq, Ord)

data BindingName
  = BindingName BindingSymbol
  | InputBinding
  deriving stock (Show, Eq, Ord)

newtype TemplateString a = TemplateString [Either (Selector a) Text]
  deriving stock (Show)

instance Functor TemplateString where
  fmap f (TemplateString ps) = TemplateString $ fmap (bimap (fmap f) id) ps

instance Foldable TemplateString where
  foldMap f (TemplateString ps) = foldMap (either (foldMap f) mempty) ps

instance Traversable TemplateString where
  traverse f (TemplateString ps) = TemplateString <$> traverse (bitraverse (traverse f) pure) ps

type Bindings = Map BindingSymbol Chunk

type BindingDeclarations = Map BindingSymbol (Pos, ChunkType)

data ShellMode
  = Normal
  | NullStdin
  deriving stock (Show)

data Pattern p
  = -- | Bind into a regular binding name, e.g. `-> %foo`
    BindingPattern p BindingSymbol
  | -- | Bind using a pattern string, e.g. `-> "%foo-%bar.%ext"`
    PatternString p BindingDeclarations Regex
  | -- | Bind elements of a list, e.g. `-> [%foo, _, %bar]`
    PatternList p [Pattern p]
  deriving stock (Show, Functor, Foldable, Traversable)

data PatternElem p
  = PatternText p Text
  | PatternBinding p BindingSymbol
  deriving stock (Show, Functor, Foldable, Traversable)

data Selector p
  = Compose p (NonEmpty (Selector p))
  | SplitFields p Text {- delimeter -}
  | SplitLines p
  | Chars p
  | SplitWords p
  | Regex p Regex BindingDeclarations
  | RegexMatches p
  | RegexGroups p Regex BindingDeclarations
  | ListOf p (Selector p)
  | Filter p (Selector p)
  | Not p (Selector p)
  | Splat p
  | At p Int
  | Take p Int (Selector p)
  | TakeEnd p Int (Selector p)
  | Drop p Int (Selector p)
  | DropEnd p Int (Selector p)
  | Reversed p (Selector p)
  | Contains p Text
  | Action p (Expr p)
  | ParseJSON p
  | Cast p
  | Id p
  | Empty p
  | Prompt p
  | File p (Selector p {- filepath -})
  | DebugTrace p (Selector p)
  deriving stock (Show, Functor, Foldable, Traversable)

instance Tagged (Selector p) p where
  tag = \case
    Compose p _ -> p
    SplitFields p _ -> p
    SplitLines p -> p
    Chars p -> p
    SplitWords p -> p
    Regex p _ _ -> p
    RegexMatches p -> p
    RegexGroups p _ _ -> p
    ListOf p _ -> p
    Filter p _ -> p
    Not p _ -> p
    Splat p -> p
    At p _ -> p
    Take p _ _ -> p
    TakeEnd p _ _ -> p
    Drop p _ _ -> p
    DropEnd p _ _ -> p
    Reversed p _ -> p
    Contains p _ -> p
    Action p _ -> p
    ParseJSON p -> p
    Cast p -> p
    Id p -> p
    Empty p -> p
    Prompt p -> p
    File p _ -> p
    DebugTrace p _ -> p

  setTag p = \case
    Compose _ x -> Compose p x
    SplitFields _ x -> SplitFields p x
    SplitLines _ -> SplitLines p
    Chars _ -> Chars p
    SplitWords _ -> SplitWords p
    Regex _ x y -> Regex p x y
    RegexMatches _ -> RegexMatches p
    RegexGroups _ x y -> RegexGroups p x y
    ListOf _ x -> ListOf p x
    Filter _ x -> Filter p x
    Not _ x -> Not p x
    Splat _ -> Splat p
    At _ x -> At p x
    Take _ x y -> Take p x y
    TakeEnd _ x y -> TakeEnd p x y
    Drop _ x y -> Drop p x y
    DropEnd _ x y -> DropEnd p x y
    Reversed _ x -> Reversed p x
    Contains _ x -> Contains p x
    Action _ x -> Action p x
    ParseJSON _ -> ParseJSON p
    Cast _ -> Cast p
    Id _ -> Id p
    Empty _ -> Empty p
    Prompt _ -> Prompt p
    File _ fp -> File p fp
    DebugTrace _ x -> DebugTrace p x

data Chunk
  = TextChunk Text
  | ListChunk [Chunk]
  | NumberChunk NumberT
  | RegexMatchChunk Re.Match
  | JsonChunk Value
  | RecordChunk (Map BindingSymbol Chunk)
  | UnitChunk
  deriving (Eq, Ord)

instance Show Chunk where
  show = \case
    TextChunk txt -> show txt
    ListChunk chs -> show chs
    NumberChunk n -> show n
    RegexMatchChunk m -> show $ m ^.. Re.matchAndGroups
    JsonChunk v -> BSC.unpack $ Aeson.encode v
    RecordChunk m -> show m
    UnitChunk -> "null"

renderChunk :: Chunk -> Text
renderChunk = \case
  TextChunk txt -> txt
  ListChunk chs -> Text.pack . show $ renderChunk <$> chs
  NumberChunk n -> case n of
    IntNumber i -> Text.pack $ show i
    DoubleNumber d -> Text.pack $ show d
  RegexMatchChunk _m -> error "Can't render p regex match chunk"
  JsonChunk v -> Text.pack $ BSC.unpack $ Aeson.encode v
  RecordChunk fields ->
    "{" <> Text.intercalate ", " (Map.toList fields <&> \(BindingSymbol k, v) -> k <> ": " <> renderChunk v) <> "}"
  UnitChunk -> "null"

data ChunkType
  = TextType
  | ListType ChunkType
  | NumberType
  | RegexMatchType
  | JsonType
  | RecordType (Map BindingSymbol ChunkType)
  | NullType
  deriving (Show, Eq)

type TaggedExpr = Expr Pos

data MathBinOp = Plus | Minus | Multiply | Divide | Modulo | Power
  deriving stock (Show, Eq, Ord)

data Expr p
  = Modify p (Selector p {- <- selector -}) (Selector p {- <- modifier -})
  | Binding p BindingName
  | Str p (TemplateString p)
  | Number p (NumberT)
  | StrConcat p (Selector p)
  | StrAppend p (Selector p) (Selector p)
  | Intersperse p (NonEmpty (Selector p))
  | Comma p (Selector p) (Selector p)
  | Count p (Selector p)
  | Shell p (TemplateString p) ShellMode
  | MathBinOp p MathBinOp (Selector p) (Selector p)
  | Record p (Map BindingSymbol (Selector p))
  | Cycle p (Selector p)
  | Index p
  | Uniq p (Selector p)
  | Pattern p (Pattern p)
  | Select p [(Selector p, Selector p)]
  deriving stock (Show, Functor, Foldable, Traversable)

instance Tagged (Expr p) p where
  tag = \case
    Modify p _ _ -> p
    Binding p _ -> p
    Str p _ -> p
    Number p _ -> p
    StrConcat p _ -> p
    StrAppend p _ _ -> p
    Intersperse p _ -> p
    Comma p _ _ -> p
    Count p _ -> p
    MathBinOp p _ _ _ -> p
    Shell p _ _ -> p
    Record p _ -> p
    Cycle p _ -> p
    Pattern p _ -> p
    Index p -> p
    Uniq p _ -> p
    Select p _ -> p
  setTag p = \case
    Modify _ x y -> Modify p x y
    Binding _ x -> Binding p x
    Str _ x -> Str p x
    Number _ x -> Number p x
    StrConcat _ x -> StrConcat p x
    StrAppend _ x y -> StrAppend p x y
    Intersperse _ x -> Intersperse p x
    Comma _ x y -> Comma p x y
    Count _ x -> Count p x
    MathBinOp _ x y z -> MathBinOp p x y z
    Shell _ x y -> Shell p x y
    Record _ x -> Record p x
    Cycle _ x -> Cycle p x
    Pattern _ pat -> Pattern p pat
    Index _ -> Index p
    Uniq _ x -> Uniq p x
    Select _ x -> Select p x

data NumberT
  = IntNumber Int
  | DoubleNumber Double
  deriving stock (Show, Eq, Ord)

renderNumber :: NumberT -> Text
renderNumber = \case
  IntNumber i -> Text.pack $ show i
  DoubleNumber d -> Text.pack $ show d

makePrisms ''Chunk
