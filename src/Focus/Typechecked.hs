{-# LANGUAGE DataKinds #-}

module Focus.Typechecked
  ( TypedSelector (..),
    SomeTypedSelector (..),
    ChunkTypeT (..),
    Typ,
    UVar,
  )
where

import Control.Unification (UTerm, Unifiable (..))
import Control.Unification.STVar qualified as Unify
import Data.Text (Text)
import Error.Diagnose qualified as D
import Focus.Tagged (Tagged (..))
import Focus.Untyped hiding (Selector (..))
import Text.Regex.PCRE.Light qualified as Re

type UVar s = Unify.STVar s (ChunkTypeT D.Position)

type Typ s = UTerm (ChunkTypeT D.Position) (UVar s)

data ChunkTypeT a r
  = Arrow a r r
  | TextTypeT a
  | ListTypeT a r
  | NumberTypeT a
  | RegexMatchTypeT a
  deriving stock (Show, Eq, Functor, Foldable, Traversable)

instance Unifiable (ChunkTypeT a) where
  zipMatch = \cases
    (Arrow pos x y) (Arrow _ x' y') -> Just (Arrow pos (Right (x, x')) (Right (y, y')))
    (TextTypeT pos) TextTypeT {} -> Just (TextTypeT pos)
    (ListTypeT pos x) (ListTypeT _ y) -> Just (ListTypeT pos $ Right (x, y))
    (NumberTypeT pos) NumberTypeT {} -> Just $ NumberTypeT pos
    (RegexMatchTypeT pos) RegexMatchTypeT {} -> Just $ RegexMatchTypeT pos
    TextTypeT {} _ -> Nothing
    ListTypeT {} _ -> Nothing
    NumberTypeT {} _ -> Nothing
    RegexMatchTypeT {} _ -> Nothing
    Arrow {} _ -> Nothing

instance Tagged (ChunkTypeT a r) a where
  tag = \case
    Arrow pos _ _ -> pos
    TextTypeT pos -> pos
    ListTypeT pos _ -> pos
    NumberTypeT pos -> pos
    RegexMatchTypeT pos -> pos

data TypedSelector (i :: ChunkType) (o :: ChunkType) a where
  Compose :: a -> TypedSelector i m a -> TypedSelector m o a -> TypedSelector i o a
  SplitFields :: a -> Text -> TypedSelector TextType TextType a
  SplitLines :: a -> TypedSelector TextType TextType a
  SplitWords :: a -> TypedSelector TextType TextType a
  Regex :: a -> Re.Regex -> TypedSelector TextType TextType a
  RegexMatches :: a -> TypedSelector RegexMatchType TextType a
  RegexGroups :: a -> Re.Regex -> TypedSelector TextType TextType a
  ListOf :: a -> TypedSelector i o a -> TypedSelector i (ListType o) a
  FilterBy :: a -> TypedSelector i o a -> TypedSelector i o a
  Splat :: a -> TypedSelector (ListType t) t a
  Shell :: a -> BindingString -> ShellMode -> TypedSelector TextType TextType a
  At :: a -> Int -> TypedSelector (ListType t) t a

instance Tagged (TypedSelector i o a) a where
  tag = \case
    Compose pos _ _ -> pos
    SplitFields pos _ -> pos
    SplitLines pos -> pos
    SplitWords pos -> pos
    Regex pos _ -> pos
    RegexMatches pos -> pos
    RegexGroups pos _ -> pos
    ListOf pos _ -> pos
    Splat pos -> pos
    FilterBy pos _ -> pos
    Shell pos _ _ -> pos
    At pos _ -> pos

deriving instance Functor (TypedSelector i o)

data SomeTypedSelector a where
  SomeTypedSelector :: TypedSelector i o a -> SomeTypedSelector a

instance Tagged (SomeTypedSelector a) a where
  tag (SomeTypedSelector s) = tag s
