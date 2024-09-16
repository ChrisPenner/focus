{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Focus.Typechecker.Types
  ( TypedSelector (..),
    SomeTypedSelector (..),
    inputType,
    outputType,
  )
where

import Control.Lens.Regex.Text qualified as Re
import Data.Text (Text)
import Focus.Tagged (Tagged (..))
import Focus.Types

data TypedSelector (i :: ChunkType) (o :: ChunkType) a where
  Compose :: a -> TypedSelector i m a -> TypedSelector m o a -> TypedSelector i o a
  SplitFields :: a -> Text -> TypedSelector TextType TextType a
  SplitLines :: a -> TypedSelector TextType TextType a
  SplitWords :: a -> TypedSelector TextType TextType a
  Regex :: a -> Re.Regex -> TypedSelector TextType RegexMatchType a
  RegexMatches :: a -> TypedSelector RegexMatchType TextType a
  RegexGroups :: a -> TypedSelector RegexMatchType TextType a
  ListOf :: a -> TypedSelector i o a -> TypedSelector i (ListType o) a
  Shell :: a -> Text -> TypedSelector TextType TextType a
  At :: a -> Int -> TypedSelector (ListType t) t a

instance Tagged (TypedSelector i o) where
  tag = \case
    Compose pos _ _ -> pos
    SplitFields pos _ -> pos
    SplitLines pos -> pos
    SplitWords pos -> pos
    Regex pos _ -> pos
    RegexMatches pos -> pos
    RegexGroups pos -> pos
    ListOf pos _ -> pos
    Shell pos _ -> pos
    At pos _ -> pos

deriving instance Functor (TypedSelector i o)

data SomeTypedSelector a where
  SomeTypedSelector :: TypedSelector i o a -> SomeTypedSelector a

instance Tagged SomeTypedSelector where
  tag (SomeTypedSelector s) = tag s

inputType :: TypedSelector i o a -> ChunkTypeF i
inputType = \case
  Compose _pos a _ -> inputType a
  SplitFields _pos _ -> TextTypeF
  SplitLines _pos -> TextTypeF
  SplitWords _pos -> TextTypeF
  Regex _pos _ -> TextTypeF
  RegexMatches _pos -> RegexMatchTypeF
  RegexGroups _pos -> RegexMatchTypeF
  ListOf _pos t -> inputType t
  Shell _pos _ -> TextTypeF
  At _pos _ -> ListTypeF (AnyTypeF)

outputType :: TypedSelector i o a -> ChunkTypeF o
outputType = \case
  Compose _pos _ b -> outputType b
  SplitFields _pos _ -> TextTypeF
  SplitLines _pos -> TextTypeF
  SplitWords _pos -> TextTypeF
  Regex _pos _ -> RegexMatchTypeF
  RegexMatches _pos -> TextTypeF
  RegexGroups _pos -> TextTypeF
  ListOf _pos t -> ListTypeF (outputType t)
  Shell _pos _ -> TextTypeF
  At _pos _ -> AnyTypeF
