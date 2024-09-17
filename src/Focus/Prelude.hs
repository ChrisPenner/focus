module Focus.Prelude
  ( tShow,
    (&),
    (<&>),
    module X,
  )
where

import Control.Applicative as X
import Control.Monad as X
import Data.Foldable as X
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as Text
import Focus.Orphans ()

tShow :: (Show a) => a -> Text
tShow = Text.pack . show
