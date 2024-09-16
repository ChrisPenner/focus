module Focus.Types
  ( Chunk (..),
    ChunkType (..),
    textChunk_,
    listChunk_,
    numberChunk_,
  )
where

import Control.Lens
import Data.Text (Text)

data Chunk
  = TextChunk Text
  | ListChunk [Chunk]
  | NumberChunk Double
  deriving (Show, Eq)

textChunk_ :: Prism' Chunk Text
textChunk_ = prism' TextChunk $ \case
  TextChunk txt -> Just txt
  _ -> Nothing

listChunk_ :: Prism' Chunk [Chunk]
listChunk_ = prism' ListChunk $ \case
  ListChunk chs -> Just chs
  _ -> Nothing

numberChunk_ :: Prism' Chunk Double
numberChunk_ = prism' NumberChunk $ \case
  NumberChunk n -> Just n
  _ -> Nothing

data ChunkType
  = TextType
  | ListType ChunkType
  | NumberType
  | AnyType
  deriving (Show, Eq)
