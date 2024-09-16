module Focus.Tagged (Tagged (..)) where

class Tagged f where
  tag :: f a -> a
