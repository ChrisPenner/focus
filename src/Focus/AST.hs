module Focus.AST (AST (..), Selector (..), typecheckSelector, typecheckAST) where

import Data.Foldable1 (foldrM1)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Focus.Types (ChunkType (..))

data AST
  = Compose (NonEmpty Selector)
  deriving stock (Show)

-- data ASTF r = ComposeF (NonEmpty r)
--   deriving stock (Show)

data Selector
  = SplitFields Text {- delimeter -}
  | SplitLines
  | SplitWords
  | Regex Text {- pattern -}
  | ListOf AST
  | Shell Text
  | At Int
  deriving stock (Show)

-- data SelectorF r
--   = SplitFieldsF Text
--   | SplitLinesF
--   | SplitWordsF
--   | RegexF Text
--   | ListOfF r
--   | ShellF Text
--   | AtF Int
--   deriving stock (Show)

data TypeError = TypeMismatch (Selector, ChunkType) (Selector, ChunkType)
  deriving stock (Show)

typecheckAST :: AST -> Either TypeError (ChunkType {- input -}, ChunkType {- output -})
typecheckAST = \case
  Compose (selectors) -> do
    selectorTypes <- traverse typecheckSelector selectors

    snd <$> foldrM1 checkTypes (NE.zip selectors selectorTypes)
  where
    checkTypes :: (Selector, (ChunkType, ChunkType)) -> (Selector, (ChunkType, ChunkType)) -> Either TypeError (Selector, (ChunkType, ChunkType))
    checkTypes (sl, (i, l)) (sr, (r, o)) =
      if l == r
        then
          -- Keep the left selector in the list because we're doing a foldr
          Right (sl, (i, o))
        else Left $ TypeMismatch (sl, l) (sr, r)

typecheckSelector :: Selector -> Either TypeError (ChunkType {- input -}, ChunkType {- output -})
typecheckSelector = \case
  SplitFields _ -> pure (TextType, TextType)
  SplitLines -> pure (TextType, TextType)
  SplitWords -> pure (TextType, TextType)
  Regex _ -> pure (TextType, TextType)
  ListOf ast -> do
    (input, output) <- typecheckAST ast
    pure (input, ListType output)
  Shell _ -> pure (TextType, TextType)
  At _ ->
    -- TODO: Guess I have to do proper unification here :P
    pure (ListType AnyType, AnyType)
