{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Focus.Types
  ( BindingName,
    Bindings,
    BindingDeclarations,
    Chunk (..),
    Focus (..),
    Focusable,
    FocusM (..),
    SelectorError (..),
    ShellMode (..),
    Typ,
    UVar,
    ChunkType (..),
    ChunkTypeT (..),
    TypeErrorReport,
    ReturnArity (..),
    FocusEnv (..),
    FocusOpts (..),
    focusOpts,
    focusBindings,
    handleErr,
    (>.>),
    castNumber,
  )
where

import Control.Category (Category (..))
import Control.Category qualified as Cat
import Control.Lens
import Control.Monad.Fix (MonadFix (..))
import Control.Monad.RWS.CPS (MonadReader (..))
import Control.Monad.Reader (ReaderT)
import Control.Monad.State.Lazy
import Control.Unification (UTerm, Unifiable)
import Control.Unification.STVar qualified as Unify
import Control.Unification.Types (Unifiable (..))
import Data.Aeson qualified as Aeson
import Data.Scientific qualified as S
import Data.Text (Text)
import Data.Text qualified as Text
import Error.Diagnose qualified as D
import Focus.Command (CommandF (..), CommandT (..), IsCmd (..))
import Focus.Prelude
import Focus.Tagged (Tagged (..))
import Focus.Untyped
import Text.Read (readMaybe)
import Prelude hiding (reads)

data SelectorError
  = ShellError Text
  | BindingError Pos Text
  | JsonParseError Pos Text {- < chunk -} Text {- < error -}
  | CastError Pos ChunkType {- destination type -} Chunk {- source chunk -}

newtype FocusM a = FocusM {runFocusM :: ReaderT FocusEnv IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFix, MonadReader FocusEnv)

data FocusOpts = FocusOpts
  { _handleErr :: SelectorError -> IO ()
  }

data FocusEnv = FocusEnv
  { _focusBindings :: Bindings,
    _focusOpts :: FocusOpts
  }

makeLenses ''FocusEnv
makeLenses ''FocusOpts

type Focusable m = (MonadReader FocusEnv m, MonadIO m, MonadFix m)

data Focus (cmd :: CommandT) i o where
  ViewFocus :: (forall m r. (Monoid r, Focusable m) => LensLike m i r o r) -> Focus 'ViewT i o
  ModifyFocus :: (forall m. (Focusable m) => LensLike' m i o) -> Focus 'ModifyT i o

instance (IsCmd cmd) => Category (Focus cmd) where
  id = case getCmd @cmd of
    ViewF -> ViewFocus $ \f s -> f s
    ModifyF -> ModifyFocus $ Cat.id
  (ModifyFocus l) . (ModifyFocus r) = ModifyFocus $ r Cat.. l
  (ViewFocus l) . (ViewFocus r) = ViewFocus $ r Cat.. l

(>.>) :: (IsCmd cmd) => Focus cmd a b -> Focus cmd b c -> Focus cmd a c
(>.>) = flip (Cat..)

type UVar s = Unify.STVar s (ChunkTypeT D.Position)

type Typ s = UTerm (ChunkTypeT D.Position) (UVar s)

data ReturnArity = Affine | Exactly Int | Any
  deriving stock (Show, Eq, Ord)

data ChunkTypeT a r
  = Arrow a r r
  | TextTypeT a
  | ListTypeT a r
  | NumberTypeT a
  | RegexMatchTypeT a
  | JsonTypeT a
  | CastableTypeT a r
  deriving stock (Show, Eq, Functor, Foldable, Traversable)

instance Unifiable (ChunkTypeT a) where
  zipMatch = \cases
    -- TODO: this isn't quite right
    (CastableTypeT pos x) (CastableTypeT _ y) -> Just (CastableTypeT pos (Right (x, y)))
    (CastableTypeT pos _) (NumberTypeT {}) -> Just (NumberTypeT pos)
    (TextTypeT pos) (CastableTypeT _ _) -> Just (TextTypeT pos)
    (CastableTypeT pos _) (TextTypeT {}) -> Just (TextTypeT pos)
    (NumberTypeT pos) (CastableTypeT _ _) -> Just (NumberTypeT pos)
    (NumberTypeT pos) NumberTypeT {} -> Just $ NumberTypeT pos
    (Arrow pos x y) (Arrow _ x' y') -> Just (Arrow pos (Right (x, x')) (Right (y, y')))
    (TextTypeT pos) (TextTypeT _pos) -> Just (TextTypeT pos)
    (ListTypeT pos x) (ListTypeT _ y) -> Just (ListTypeT pos $ Right (x, y))
    (RegexMatchTypeT pos) RegexMatchTypeT {} -> Just $ RegexMatchTypeT pos
    CastableTypeT {} _ -> Nothing
    TextTypeT {} _ -> Nothing
    ListTypeT {} _ -> Nothing
    NumberTypeT {} _ -> Nothing
    RegexMatchTypeT {} _ -> Nothing
    Arrow {} _ -> Nothing
    JsonTypeT {} _ -> Nothing

instance Tagged (ChunkTypeT a r) a where
  tag = \case
    Arrow pos _ _ -> pos
    TextTypeT pos -> pos
    ListTypeT pos _ -> pos
    NumberTypeT pos -> pos
    RegexMatchTypeT pos -> pos
    JsonTypeT pos -> pos
    CastableTypeT pos _ -> pos

type TypeErrorReport = D.Report Text

castNumber :: Chunk -> Maybe NumberT
castNumber inp = case inp of
  TextChunk txt -> castText txt
  JsonChunk (Aeson.String txt) -> castText txt
  NumberChunk num -> pure $ num
  JsonChunk (Aeson.Number scientific) -> case S.floatingOrInteger scientific of
    Left double -> pure $ DoubleNumber double
    Right int -> pure $ IntNumber int
  _ -> Nothing
  where
    castText :: Text -> Maybe NumberT
    castText txt =
      case readMaybe (Text.unpack txt) of
        Just i -> Just $ IntNumber i
        Nothing -> case readMaybe (Text.unpack txt) of
          Just d -> Just $ DoubleNumber d
          Nothing -> Nothing
