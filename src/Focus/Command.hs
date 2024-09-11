module Focus.Command (Command (..)) where

import Data.Text (Text)

type Script = Text

data Command
  = View Script
  | Over Script Text
  | Set Script Text
  deriving stock (Show)
