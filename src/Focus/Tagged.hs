{-# LANGUAGE FunctionalDependencies #-}

module Focus.Tagged (Tagged (..)) where

class Tagged t a | t -> a where
  tag :: t -> a
