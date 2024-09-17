{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Focus.Typechecker.Types
  ( TypedSelector (..),
    SomeTypedSelector (..),
    inputType,
    outputType,
    Chunk (..),
    ChunkType (..),
    ChunkTypeF (..),
    getChunkType,
    _TextChunk,
    _ListChunk,
    _NumberChunk,
    _RegexMatchChunk,
    renderType,
    unifies,
    ChunkTypeT (..),
    Typ,
    UVar,
    arrow,
    textType,
    listType,
    numberType,
    regexMatchType,
  )
where

import Control.Lens
import Control.Lens.Regex.Text qualified as Re
import Control.Unification (UTerm (..), Unifiable (..))
import Control.Unification.STVar qualified as Unify
import Data.Text (Text)
import Data.Type.Equality (TestEquality (..))
import Focus.Tagged (Tagged (..))

data Chunk
  = TextChunk Text
  | ListChunk [Chunk]
  | NumberChunk Double
  | RegexMatchChunk Re.Match

makePrisms ''Chunk

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
  | AnyType
  | RegexMatchType
  deriving (Show, Eq)

data ChunkTypeF (ct :: ChunkType) where
  TextTypeF :: ChunkTypeF 'TextType
  ListTypeF :: ChunkTypeF t -> ChunkTypeF ('ListType t)
  NumberTypeF :: ChunkTypeF 'NumberType
  AnyTypeF :: ChunkTypeF any
  RegexMatchTypeF :: ChunkTypeF 'RegexMatchType

data ChunkTypeT r
  = Arrow r r
  | TextTypeT
  | ListTypeT r
  | NumberTypeT
  | RegexMatchTypeT
  deriving stock (Show, Eq, Functor, Foldable, Traversable)

type UVar s = Unify.STVar s ChunkTypeT

type Typ s = UTerm ChunkTypeT (UVar s)

arrow :: Typ v -> Typ v -> Typ v
arrow l r = UTerm $ Arrow l r

textType :: Typ v
textType = UTerm TextTypeT

listType :: Typ v -> Typ v
listType t = UTerm $ ListTypeT t

numberType :: Typ v
numberType = UTerm NumberTypeT

regexMatchType :: Typ v
regexMatchType = UTerm RegexMatchTypeT

instance Unifiable ChunkTypeT where
  zipMatch = \cases
    (Arrow x y) (Arrow x' y') -> Just (Arrow (Right (x, x')) (Right (y, y')))
    TextTypeT TextTypeT -> Just TextTypeT
    (ListTypeT x) (ListTypeT y) -> Just (ListTypeT $ Right (x, y))
    NumberTypeT NumberTypeT -> Just NumberTypeT
    RegexMatchTypeT RegexMatchTypeT -> Just RegexMatchTypeT
    TextTypeT _ -> Nothing
    ListTypeT {} _ -> Nothing
    NumberTypeT _ -> Nothing
    RegexMatchTypeT _ -> Nothing
    Arrow {} _ -> Nothing

getChunkType :: ChunkTypeF x -> ChunkType
getChunkType = \case
  TextTypeF -> TextType
  ListTypeF t -> ListType (getChunkType t)
  NumberTypeF -> NumberType
  AnyTypeF -> AnyType
  RegexMatchTypeF -> RegexMatchType

instance TestEquality ChunkTypeF where
  testEquality TextTypeF TextTypeF = Just Refl
  testEquality (ListTypeF x) (ListTypeF y) =
    case x `testEquality` y of
      Just Refl -> Just Refl
      Nothing -> Nothing
  testEquality NumberTypeF NumberTypeF = Just Refl
  testEquality AnyTypeF AnyTypeF = error "AnyTypeF should not be compared"
  testEquality RegexMatchTypeF RegexMatchTypeF = Just Refl
  testEquality _ _ = Nothing

unifies :: ChunkType -> ChunkType -> Bool
unifies AnyType _ = True
unifies _ AnyType = True
unifies (ListType x) (ListType y) = x `unifies` y
unifies x y = x == y

renderType :: ChunkType -> Text
renderType = \case
  TextType -> "text"
  ListType t -> "[" <> renderType t <> "]"
  NumberType -> "number"
  RegexMatchType -> "regex-match"
  AnyType -> "any"

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
