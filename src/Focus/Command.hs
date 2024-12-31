{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Focus.Command
  ( Command (..),
    CommandT (..),
    CommandF (..),
    IsCmd (..),
    InputLocation (..),
    OutputLocation (..),
    ScriptLocation (..),
  )
where

import Data.Text (Text)

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

data InputLocation
  = StdIn
  | InputFile FilePath
  deriving stock (Show, Eq, Ord)

data OutputLocation
  = StdOut
  | OutputFile FilePath
  deriving stock (Show, Eq, Ord)

data ScriptLocation
  = ScriptFile FilePath
  | ScriptLiteral Text
  deriving stock (Show, Eq, Ord)

data Command
  = Command ScriptLocation [InputLocation]
  deriving stock (Show)
