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
  )
where

import Control.Lens
import Control.Lens.Regex.Text qualified as Re
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Text (Text)
import Error.Diagnose qualified as D
import Focus.Tagged (Tagged (..))
import Text.Regex.PCRE.Heavy (Regex)

type Pos = D.Position

type TaggedSelector = Selector Pos

newtype BindingName = BindingName Text
  deriving stock (Show, Eq, Ord)

newtype BindingString = BindingString [Either (BindingName, Pos) Text]
  deriving stock (Show, Eq, Ord)

renderBindingString :: BindingString -> Text
renderBindingString (BindingString xs) =
  xs
    <&> \case
      Left (BindingName name, _pos) -> "%{" <> name <> "}"
      Right txt -> txt
    & mconcat

type Bindings = Map BindingName Chunk

type BindingDeclarations = Map Text (Pos, ChunkType)

data ShellMode
  = Normal
  | NullStdin
  deriving stock (Show)

data Selector a
  = Compose a (NonEmpty (Selector a))
  | SplitFields a Text {- delimeter -}
  | SplitLines a
  | SplitWords a
  | Regex a Regex BindingDeclarations
  | RegexMatches a
  | RegexGroups a Regex BindingDeclarations
  | ListOf a (Selector a)
  | FilterBy a (Selector a)
  | Splat a
  | Shell a BindingString ShellMode
  | At a Int
  deriving stock (Show, Functor, Foldable, Traversable)

instance Tagged (Selector a) a where
  tag = \case
    Compose a _ -> a
    SplitFields a _ -> a
    SplitLines a -> a
    SplitWords a -> a
    Regex a _ _ -> a
    RegexMatches a -> a
    RegexGroups a _ _ -> a
    ListOf a _ -> a
    FilterBy a _ -> a
    Splat a -> a
    Shell a _ _ -> a
    At a _ -> a

data Chunk
  = TextChunk Text
  | ListChunk [Chunk]
  | NumberChunk Double
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

makePrisms ''Chunk
