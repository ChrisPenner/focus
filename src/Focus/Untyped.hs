{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Focus.Untyped
  ( Selector (..),
    TaggedSelector,
    ShellMode (..),
    BindingName,
    Bindings,
    BindingDeclarations,
    Chunk (..),
    ChunkType (..),
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

type TaggedSelector = Selector D.Position

type BindingName = Text

type Bindings = Map BindingName Chunk

type BindingDeclarations = Map Text (D.Position, ChunkType)

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
  | RegexGroups a
  | ListOf a (Selector a)
  | FilterBy a (Selector a)
  | Splat a
  | Shell a Text ShellMode
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
    RegexGroups a -> a
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
