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
    TypedSelector (..),
    ShellMode (..),
    Typ,
    UVar,
    SomeTypedSelector (..),
    ChunkType (..),
    ChunkTypeT (..),
  )
where

import Control.Lens
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Except (ExceptT)
import Control.Monad.Fix (MonadFix (..))
import Control.Monad.RWS.CPS (MonadReader (..))
import Control.Monad.Reader (ReaderT)
import Control.Monad.State.Lazy
import Data.Text (Text)
import Focus.Command (CommandT (..))
import Focus.Prelude
import Focus.Typechecked
import Focus.Untyped
import Prelude hiding (reads)

data SelectorError
  = ShellError Text
  | BindingError Pos Text

newtype FocusM a = FocusM {runFocusM :: ExceptT SelectorError (ReaderT Bindings IO) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadError SelectorError, MonadFix, MonadReader Bindings)

type Focusable m = (MonadReader Bindings m, MonadIO m, MonadFix m, MonadError SelectorError m)

data Focus (cmd :: CommandT) i o where
  ViewFocus :: (forall m. (Focusable m) => (o -> m ()) -> i -> m ()) -> Focus 'ViewT i o
  ModifyFocus :: (forall m. (Focusable m) => LensLike' m i o) -> Focus 'ModifyT i o
