{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Focus.Command
  ( Command (..),
    CommandT (..),
    CommandF (..),
  )
where

import Data.Text (Text)

type Script = Text

data CommandT = ViewT | ModifyT | SetT

data CommandF (cmd :: CommandT) where
  ViewF :: CommandF 'ViewT
  ModifyF :: CommandF 'ModifyT

data Command
  = View Script [FilePath]
  | Modify Script Text [FilePath]
  | Set Script Text [FilePath]
  deriving stock (Show)
