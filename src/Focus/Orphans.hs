{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Focus.Orphans () where

import Control.Monad.Coroutine (Coroutine)
import Control.Monad.Coroutine qualified as Co
import Control.Monad.Fix
import Control.Monad.Reader
import Data.Functor ((<&>))
import Data.Void (Void)
import Error.Diagnose qualified as D
import Error.Diagnose.Compat.Megaparsec (HasHints (..))
import GHC.Generics (Generic)
import Text.Regex.PCRE.Light.Base qualified as RE
import UnliftIO (MonadUnliftIO (..))

instance HasHints Void a where
  hints e = case e of {}

-- Combine positions to get the span of both positions.
instance Semigroup D.Position where
  D.Position {D.begin = (startLine, startCol), D.end = (endLine, endCol), D.file = file1} <> D.Position {D.begin = (startLine2, startCol2), D.end = (endLine2, endCol2), D.file = file2}
    | file1 /= file2 = error "Cannot combine positions from different files"
    | otherwise = D.Position {D.begin = min (startLine, startCol) (startLine2, startCol2), D.end = max (endLine, endCol) (endLine2, endCol2), D.file = file1}

deriving instance Generic RE.Regex

instance (Functor f, MonadReader e m) => MonadReader e (Coroutine f m) where
  ask = lift ask
  local f = Co.mapMonad (local f)

instance (MonadFix m, Functor f) => MonadFix (Coroutine f m) where
  mfix f = Co.Coroutine $ mfix $ \case
    Left sm -> pure . Left $ sm <&> \co -> co >>= f
    Right r -> Co.resume (f r)
