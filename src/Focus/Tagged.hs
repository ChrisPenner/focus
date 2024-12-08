{-# LANGUAGE FunctionalDependencies #-}

module Focus.Tagged (Tagged (..), tag_) where

import Control.Lens (Lens', lens)

class Tagged t a | t -> a where
  tag :: t -> a
  setTag :: a -> t -> t

tag_ :: (Tagged t a) => Lens' t a
tag_ = lens tag (flip setTag)
