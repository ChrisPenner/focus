module Focus.Prelude (tShow, (&), (<&>)) where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as Text
import Focus.Orphans ()

tShow :: (Show a) => a -> Text
tShow = Text.pack . show
