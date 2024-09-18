{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Focus.Command
  ( Command (..),
    CommandT (..),
    CommandF (..),
    IsCmd (..),
  )
where

import Data.Text (Text)

type Script = Text

data CommandT = ViewT | ModifyT

class IsCmd (cmd :: CommandT) where
  getCmd :: CommandF cmd

instance IsCmd 'ViewT where
  getCmd = ViewF

instance IsCmd 'ModifyT where
  getCmd = ModifyF

data CommandF (cmd :: CommandT) where
  ViewF :: CommandF 'ViewT
  ModifyF :: CommandF 'ModifyT

data Command
  = View Script [FilePath]
  | Modify Script Text [FilePath]
  | Set Script Text [FilePath]
  deriving stock (Show)
