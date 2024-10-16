{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Focus.Untyped
  ( Selector (..),
    TaggedSelector,
    ShellMode (..),
    BindingName (..),
    TemplateString (..),
    PatternElem (..),
    PatternString (..),
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
  )
where

import Control.Lens
import Control.Lens.Regex.Text qualified as Re
import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.Bitraversable
import Data.ByteString.Lazy.Char8 qualified as BSC
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Text (Text)
import Data.Text qualified as Text
import Error.Diagnose qualified as D
import Focus.Tagged (Tagged (..))
import Text.Regex.PCRE.Heavy (Regex)

type Pos = D.Position

data VoidF a
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

absurdF :: VoidF a -> b
absurdF = \case {}

type TaggedSelector = Selector Pos

data BindingName
  = BindingName Text
  | InputBinding
  deriving stock (Show, Eq, Ord)

data PatternElem a
  = PatternText a Text
  | PatternBinding a Text
  deriving stock (Show, Functor, Foldable, Traversable)

newtype PatternString a = PatternString [PatternElem a]
  deriving stock (Show, Functor, Foldable, Traversable)

newtype TemplateString a = TemplateString [Either (Selector a) Text]
  deriving stock (Show)

instance Functor TemplateString where
  fmap f (TemplateString ps) = TemplateString $ fmap (bimap (fmap f) id) ps

instance Foldable TemplateString where
  foldMap f (TemplateString ps) = foldMap (either (foldMap f) mempty) ps

instance Traversable TemplateString where
  traverse f (TemplateString ps) = TemplateString <$> traverse (bitraverse (traverse f) pure) ps

type Bindings = Map Text Chunk

type BindingDeclarations = Map Text (Pos, ChunkType)

data ShellMode
  = Normal
  | NullStdin
  deriving stock (Show)

data Selector a
  = Compose a (NonEmpty (Selector a))
  | Modify a (Selector a {- <- selector -}) (Selector a {- <- modifier -})
  | SplitFields a Text {- delimeter -}
  | SplitLines a
  | SplitWords a
  | Regex a Regex BindingDeclarations
  | RegexMatches a
  | RegexGroups a Regex BindingDeclarations
  | ListOf a (Selector a)
  | Filter a (Selector a)
  | Not a (Selector a)
  | Splat a
  | At a Int
  | Take a Int (Selector a)
  | TakeEnd a Int (Selector a)
  | Drop a Int (Selector a)
  | DropEnd a Int (Selector a)
  | Contains a Text
  | Action a (Expr a)
  | ParseJSON a
  | BindingAssignment a Text
  | Cast a
  deriving stock (Show, Functor, Foldable, Traversable)

instance Tagged (Selector a) a where
  tag = \case
    Compose a _ -> a
    Modify a _ _ -> a
    SplitFields a _ -> a
    SplitLines a -> a
    SplitWords a -> a
    Regex a _ _ -> a
    RegexMatches a -> a
    RegexGroups a _ _ -> a
    ListOf a _ -> a
    Filter a _ -> a
    Not a _ -> a
    Splat a -> a
    At a _ -> a
    Take a _ _ -> a
    TakeEnd a _ _ -> a
    Drop a _ _ -> a
    DropEnd a _ _ -> a
    Contains a _ -> a
    Action a _ -> a
    ParseJSON a -> a
    BindingAssignment a _ -> a
    Cast a -> a

data Chunk
  = TextChunk Text
  | ListChunk [Chunk]
  | NumberChunk NumberT
  | RegexMatchChunk Re.Match
  | JsonChunk Value

instance Show Chunk where
  show = \case
    TextChunk txt -> show txt
    ListChunk chs -> show chs
    NumberChunk n -> show n
    RegexMatchChunk m -> show $ m ^.. Re.matchAndGroups
    JsonChunk v -> BSC.unpack $ Aeson.encode v

renderChunk :: Chunk -> Text
renderChunk = \case
  TextChunk txt -> txt
  ListChunk chs -> Text.pack . show $ renderChunk <$> chs
  NumberChunk n -> case n of
    IntNumber i -> Text.pack $ show i
    DoubleNumber d -> Text.pack $ show d
  RegexMatchChunk _m -> error "Can't render a regex match chunk"
  JsonChunk v -> Text.pack $ BSC.unpack $ Aeson.encode v

data ChunkType
  = TextType
  | ListType ChunkType
  | NumberType
  | RegexMatchType
  | JsonType
  deriving (Show, Eq)

type TaggedExpr = Expr Pos

type Action a = Selector a

data MathBinOp = Plus | Minus | Multiply | Divide | Modulo | Power
  deriving stock (Show, Eq, Ord)

data Expr a
  = Binding a BindingName
  | Str a (TemplateString a)
  | Number a (NumberT)
  | StrConcat a (Action a)
  | Intersperse a (NonEmpty (Action a))
  | Comma a (Selector a) (Selector a)
  | Count a (Selector a)
  | Shell a (TemplateString a) ShellMode
  | MathBinOp a MathBinOp (Selector a) (Selector a)
  deriving stock (Show, Functor, Foldable, Traversable)

instance Tagged (Expr a) a where
  tag = \case
    Binding a _ -> a
    Str a _ -> a
    Number a _ -> a
    StrConcat a _ -> a
    Intersperse a _ -> a
    Comma a _ _ -> a
    Count a _ -> a
    MathBinOp a _ _ _ -> a
    Shell a _ _ -> a

data NumberT
  = IntNumber Int
  | DoubleNumber Double
  deriving stock (Show, Eq)

makePrisms ''Chunk
