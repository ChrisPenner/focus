module Focus.AST (AST (..), Selector (..)) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

data AST
  = Compose (NonEmpty Selector)
  deriving stock (Show)

data Selector
  = SplitFields Text {- delimeter -}
  | SplitLines
  | SplitWords
  | Regex Text {- pattern -}
  | ListOf AST
  deriving stock (Show)
