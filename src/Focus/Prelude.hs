module Focus.Prelude
  ( tShow,
    module X,
    Generic,
    foldMapM,
    foldMapMOf,
  )
where

import Control.Applicative as X
import Control.Lens
import Control.Monad as X
import Data.Foldable as X
import Data.Function as X
import Data.Functor as X
import Data.Maybe as X
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable as X
import Focus.Orphans ()
import GHC.Generics (Generic)

tShow :: (Show a) => a -> Text
tShow = Text.pack . show

foldMapMOf :: (Applicative f, Monoid m) => Fold s a -> (a -> f m) -> s -> f m
foldMapMOf l f = foldrOf l (\a r -> mappend <$> f a <*> r) (pure mempty)

foldMapM :: (Applicative f, Monoid m, Foldable t) => (a -> f m) -> t a -> f m
foldMapM = foldMapMOf folded
