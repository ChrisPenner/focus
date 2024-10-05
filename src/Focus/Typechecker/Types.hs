{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Focus.Typechecker.Types
  ( Chunk (..),
    ChunkType (..),
    renderType,
    ChunkTypeT (..),
    Typ,
    UVar,
    arrow,
    textType,
    listType,
    numberType,
    regexMatchType,
    jsonType,
    castableType,
  )
where

import Control.Unification (UTerm (..))
import Data.Set.NonEmpty qualified as NESet
import Data.Text (Text)
import Error.Diagnose qualified as D
import Focus.Types

arrow :: D.Position -> Typ v -> Typ v -> Typ v
arrow pos l r = UTerm $ Arrow (NESet.singleton pos) l r

textType :: D.Position -> Typ v
textType pos = UTerm $ TextTypeT (NESet.singleton pos)

listType :: D.Position -> Typ v -> Typ v
listType pos t = UTerm $ ListTypeT (NESet.singleton pos) t

numberType :: D.Position -> Typ v
numberType pos = UTerm $ NumberTypeT (NESet.singleton pos)

regexMatchType :: D.Position -> Typ v
regexMatchType pos = UTerm $ RegexMatchTypeT (NESet.singleton pos)

jsonType :: D.Position -> Typ v
jsonType pos = UTerm $ JsonTypeT (NESet.singleton pos)

castableType :: D.Position -> Typ v -> Typ v
castableType pos t = UTerm $ CastableTypeT (NESet.singleton pos) t

renderType :: ChunkType -> Text
renderType = \case
  TextType -> "Text"
  ListType t -> "[" <> renderType t <> "]"
  NumberType -> "Number"
  RegexMatchType -> "Regex-Match"
  JsonType -> "Json"
