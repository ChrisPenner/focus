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
    nullType,
  )
where

import Control.Lens (ifoldMap)
import Control.Unification (UTerm (..))
import Data.Map (Map)
import Data.Set.NonEmpty qualified as NESet
import Data.Text (Text)
import Error.Diagnose qualified as D
import Focus.Types

arrow :: D.Position -> Typ -> Typ -> Typ
arrow pos l r = UTerm $ Arrow (NESet.singleton pos) l r

textType :: D.Position -> Typ
textType pos = UTerm $ TextTypeT (NESet.singleton pos)

listType :: D.Position -> Typ -> Typ
listType pos t = UTerm $ ListTypeT (NESet.singleton pos) t

numberType :: D.Position -> Typ
numberType pos = UTerm $ NumberTypeT (NESet.singleton pos)

regexMatchType :: D.Position -> Typ
regexMatchType pos = UTerm $ RegexMatchTypeT (NESet.singleton pos)

jsonType :: D.Position -> Typ
jsonType pos = UTerm $ JsonTypeT (NESet.singleton pos)

recordType :: D.Position -> Map Text (Typ) -> Typ
recordType pos fields = UTerm $ RecordTypeT (NESet.singleton pos) fields

castableType :: D.Position -> Typ -> Typ
castableType pos t = UTerm $ CastableTypeT (NESet.singleton pos) t

nullType :: D.Position -> Typ
nullType pos = UTerm $ NullTypeT (NESet.singleton pos)

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
  NullType -> "Null"
