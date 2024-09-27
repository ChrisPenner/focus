{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Focus.Untyped
  ( Selector (..),
    TaggedSelector,
    ShellMode (..),
    BindingName (..),
    BindingString (..),
    renderBindingString,
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
    TaggedAction,
    NumberT (..),
    VoidF,
    absurdF,
  )
where

import Control.Lens
import Control.Lens.Regex.Text qualified as Re
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Text (Text)
import Error.Diagnose qualified as D
import Focus.Tagged (Tagged (..))
import Text.Regex.PCRE.Heavy (Regex)

type Pos = D.Position

data VoidF a
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

absurdF :: VoidF a -> b
absurdF = \case {}

type TaggedSelector = Selector VoidF Pos

data BindingName
  = BindingName Text
  | InputBinding
  deriving stock (Show, Eq, Ord)

newtype BindingString = BindingString [Either (BindingName, Pos) Text]
  deriving stock (Show, Eq, Ord)

renderBindingString :: BindingString -> Text
renderBindingString (BindingString xs) =
  xs
    <&> \case
      Left (BindingName name, _pos) -> "%{" <> name <> "}"
      Left (InputBinding, _pos) -> "%{.}"
      Right txt -> txt
    & mconcat

type Bindings = Map Text Chunk

type BindingDeclarations = Map Text (Pos, ChunkType)

data ShellMode
  = Normal
  | NullStdin
  deriving stock (Show)

data Selector (expr :: Type -> Type) a
  = Compose a (NonEmpty (Selector expr a))
  | SplitFields a Text {- delimeter -}
  | SplitLines a
  | SplitWords a
  | Regex a Regex BindingDeclarations
  | RegexMatches a
  | RegexGroups a Regex BindingDeclarations
  | ListOf a (Selector expr a)
  | Filter a (Selector expr a)
  | Not a (Selector expr a)
  | Splat a
  | Shell a BindingString ShellMode
  | At a Int
  | Take a Int (Selector expr a)
  | TakeEnd a Int (Selector expr a)
  | Drop a Int (Selector expr a)
  | DropEnd a Int (Selector expr a)
  | Contains a Text
  | Action a (expr a)
  deriving stock (Show, Functor, Foldable, Traversable)

instance Tagged (Selector expr a) a where
  tag = \case
    Compose a _ -> a
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
    Shell a _ _ -> a
    At a _ -> a
    Take a _ _ -> a
    TakeEnd a _ _ -> a
    Drop a _ _ -> a
    DropEnd a _ _ -> a
    Contains a _ -> a
    Action a _ -> a

data Chunk
  = TextChunk Text
  | ListChunk [Chunk]
  | NumberChunk NumberT
  | RegexMatchChunk Re.Match

instance Show Chunk where
  show = \case
    TextChunk txt -> show txt
    ListChunk chs -> show chs
    NumberChunk n -> show n
    RegexMatchChunk m -> show $ m ^.. Re.matchAndGroups

data ChunkType
  = TextType
  | ListType ChunkType
  | NumberType
  | RegexMatchType
  deriving (Show, Eq)

type TaggedExpr = Expr Pos

type TaggedAction = Selector Expr Pos

type Action a = Selector Expr a

data Expr a
  = Binding a BindingName
  | Str a BindingString
  | Number a (NumberT)
  | StrConcat a (Action a)
  | Intersperse a (NonEmpty (Action a))
  deriving stock (Show, Functor, Foldable, Traversable)

data NumberT
  = IntNumber Int
  | DoubleNumber Double
  deriving stock (Show, Eq)

makePrisms ''Chunk
