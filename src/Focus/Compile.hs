module Focus.Compile (compileAST, Focus (..)) where

import Control.Lens
import Data.Text (Text)
import Data.Text qualified as Text
import Focus.AST
import Focus.Types (Chunk (..))

-- data Err = TypeMismatch (ChunkType {- expected -}) (ChunkType {- actual -})

newtype Focus = Focus (Traversal' Chunk Chunk)

textChunk :: Chunk -> Text
textChunk = \case
  TextChunk txt -> txt
  actual -> error $ "Expected TextChunk, got " <> show actual

compileAST :: AST -> Focus
compileAST = \case
  Compose selectors ->
    foldr composeFT (Focus $ id) (compileSelector <$> selectors)

composeFT :: Focus -> Focus -> Focus
composeFT (Focus l) (Focus r) =
  Focus $ l . r

compileSelector :: Selector -> Focus
compileSelector = \case
  SplitFields delim ->
    Focus $ \f chunk ->
      traverse f (fmap TextChunk $ Text.splitOn delim (textChunk chunk))
        <&> TextChunk . Text.intercalate delim . fmap textChunk
  SplitLines ->
    Focus $ \f chunk ->
      traverse f (fmap TextChunk $ Text.lines (textChunk chunk))
        <&> TextChunk . Text.unlines . fmap textChunk
  SplitWords ->
    Focus $ \f chunk ->
      traverse f (fmap TextChunk $ Text.words (textChunk chunk))
        <&> TextChunk . Text.unwords . fmap textChunk
  Regex _ -> error "regex is unsupported"
