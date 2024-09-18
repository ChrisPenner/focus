{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Focus.AST
  ( Selector (..),
    TaggedSelector,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Error.Diagnose qualified as D
import Focus.Tagged (Tagged (..))
import Focus.Types
import Text.Regex.PCRE.Heavy (Regex)

type TaggedSelector = Selector D.Position

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
  | Shell a Text
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
    Shell a _ -> a
    At a _ -> a
