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
  )
where

import Control.Lens
import Control.Monad.Fix (MonadFix (..))
import Control.Monad.RWS.CPS (MonadReader (..))
import Control.Monad.Reader (ReaderT)
import Control.Monad.State.Lazy
import Control.Unification (UTerm, Unifiable)
import Control.Unification.STVar qualified as Unify
import Control.Unification.Types (Unifiable (..))
import Data.Text (Text)
import Error.Diagnose qualified as D
import Focus.Command (CommandT (..))
import Focus.Prelude
import Focus.Tagged (Tagged (..))
import Focus.Untyped
import Prelude hiding (reads)

data SelectorError
  = ShellError Text
  | BindingError Pos Text
  | JsonParseError Pos Text {- < chunk -} Text {- < error -}

newtype FocusM a = FocusM {runFocusM :: ReaderT FocusEnv IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFix, MonadReader FocusEnv)

data FocusOpts = FocusOpts
  { _handleErr :: Chunk -> SelectorError -> IO Chunk
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

type TypeErrorReport = D.Report Text
