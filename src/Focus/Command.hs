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

data CommandT = ViewT | OverT | SetT

data CommandF (cmd :: CommandT) where
  ViewF :: CommandF 'ViewT
  OverF :: CommandF 'OverT

data Command
  = View Script
  | Over Script Text
  | Set Script Text
  deriving stock (Show)
