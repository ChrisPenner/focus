module Focus.Prelude (tShow, (&), (<&>)) where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as Text

tShow :: (Show a) => a -> Text
tShow = Text.pack . show
