module Focus.Prelude
  ( tShow,
    (&),
    (<&>),
    module X,
    Generic,
  )
where

import Control.Applicative as X
import Control.Monad as X
import Data.Foldable as X
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable as X
import Focus.Orphans ()
import GHC.Generics (Generic)

tShow :: (Show a) => a -> Text
tShow = Text.pack . show
