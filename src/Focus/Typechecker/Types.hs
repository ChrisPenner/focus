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
    recordType,
    castableType,
  )
where

import Control.Lens (ifoldMap)
import Control.Unification (UTerm (..))
import Data.Map (Map)
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

recordType :: D.Position -> Map Text (Typ v) -> Typ v
recordType pos fields = UTerm $ RecordTypeT (NESet.singleton pos) fields

castableType :: D.Position -> Typ v -> Typ v
castableType pos t = UTerm $ CastableTypeT (NESet.singleton pos) t

-- | Render a 'ChunkType' as a 'Text' value.
--
-- >>> renderType TextType
-- "Text"
-- >>> renderType (ListType TextType)
-- "[Text]"
--
-- >>> import qualified Data.Map as Map
-- >>> renderType (RecordType $ Map.fromList [("foo", TextType), ("bar", ListType NumberType)])
-- "{bar: [Number],\nfoo: Text,\n}"
renderType :: ChunkType -> Text
renderType = \case
  TextType -> "Text"
  ListType t -> "[" <> renderType t <> "]"
  NumberType -> "Number"
  RegexMatchType -> "Regex-Match"
  JsonType -> "Json"
  RecordType fields -> "{" <> ifoldMap (\k v -> k <> ": " <> renderType v <> ",\n") fields <> "}"
