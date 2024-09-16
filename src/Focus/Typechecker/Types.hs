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
import Focus.Types

data TypedSelector (i :: ChunkType) (o :: ChunkType) where
  Compose :: TypedSelector i m -> TypedSelector m o -> TypedSelector i o
  SplitFields :: Text -> TypedSelector TextType TextType
  SplitLines :: TypedSelector TextType TextType
  SplitWords :: TypedSelector TextType TextType
  Regex :: Re.Regex -> TypedSelector TextType RegexMatchType
  RegexMatches :: TypedSelector RegexMatchType TextType
  RegexGroups :: TypedSelector RegexMatchType TextType
  ListOf :: TypedSelector i o -> TypedSelector i (ListType o)
  Shell :: Text -> TypedSelector TextType TextType
  At :: Int -> TypedSelector (ListType a) a

data SomeTypedSelector where
  SomeTypedSelector :: forall i o. TypedSelector i o -> SomeTypedSelector

inputType :: TypedSelector i o -> ChunkTypeF i
inputType = \case
  Compose a _ -> inputType a
  SplitFields _ -> TextTypeF
  SplitLines -> TextTypeF
  SplitWords -> TextTypeF
  Regex _ -> TextTypeF
  RegexMatches -> RegexMatchTypeF
  RegexGroups -> RegexMatchTypeF
  ListOf t -> inputType t
  Shell _ -> TextTypeF
  At _ -> ListTypeF (AnyTypeF)

outputType :: TypedSelector i o -> ChunkTypeF o
outputType = \case
  Compose _ b -> outputType b
  SplitFields _ -> TextTypeF
  SplitLines -> TextTypeF
  SplitWords -> TextTypeF
  Regex _ -> RegexMatchTypeF
  RegexMatches -> TextTypeF
  RegexGroups -> TextTypeF
  ListOf t -> ListTypeF (outputType t)
  Shell _ -> TextTypeF
  At _ -> AnyTypeF
