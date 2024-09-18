{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Focus.Types
  ( BindingName,
    Bindings,
    BindingDeclarations,
    Chunk (..),
    Focus (..),
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

newtype FocusM a = FocusM {runFocusM :: ExceptT SelectorError (ReaderT Bindings IO) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadError SelectorError, MonadFix, MonadReader Bindings)

data Focus (cmd :: CommandT) m where
  ViewFocus :: ((Chunk -> m ()) -> Chunk -> m ()) -> Focus 'ViewT m
  ModifyFocus :: LensLike' m Chunk Chunk -> Focus 'ModifyT m
  SetFocus :: LensLike' m Chunk Chunk -> Focus 'SetT m
