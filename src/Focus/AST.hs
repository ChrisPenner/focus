{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Focus.AST
  ( Selector (..),
    SelectorF (..),
    TaggedSelector,
    TypeError (..),
    TypeErrorReport,
    typecheckSelector,
    untagSelector,
  )
where

import Control.Category ((>>>))
import Control.Comonad.Cofree (Cofree)
import Control.Comonad.Cofree qualified as CF
import Control.Comonad.Trans.Cofree qualified as CofreeF
import Data.Foldable1 (foldrM1)
import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Error.Diagnose qualified as D
import Error.Diagnose qualified as Diagnose
import Focus.Types (ChunkType (..), renderType, unifies)
import Text.Regex.PCRE.Heavy (Regex)
import Text.Show.Deriving (deriveShow1)

type TaggedSelector = Cofree SelectorF D.Position

data SelectorF r
  = ComposeF (NonEmpty r)
  | SplitFieldsF Text {- delimeter -}
  | SplitLinesF
  | SplitWordsF
  | RegexF Regex {- pattern -}
  | ListOfF r
  | ShellF Text
  | AtF Int
  deriving stock (Show, Functor, Foldable, Traversable)

deriveShow1 ''SelectorF

data Selector
  = Compose (NonEmpty Selector)
  | SplitFields Text {- delimeter -}
  | SplitLines
  | SplitWords
  | Regex Regex
  | ListOf Selector
  | Shell Text
  | At Int
  deriving stock (Show)

type instance Base Selector = SelectorF

instance Recursive Selector where
  project = \case
    Compose selectors -> ComposeF selectors
    SplitFields delimeter -> SplitFieldsF delimeter
    SplitLines -> SplitLinesF
    SplitWords -> SplitWordsF
    Regex pattern -> RegexF pattern
    ListOf ast -> ListOfF ast
    Shell command -> ShellF command
    At index -> AtF index

instance Corecursive Selector where
  embed = \case
    ComposeF selectors -> Compose selectors
    SplitFieldsF delimeter -> SplitFields delimeter
    SplitLinesF -> SplitLines
    SplitWordsF -> SplitWords
    RegexF pattern -> Regex pattern
    ListOfF ast -> ListOf ast
    ShellF command -> Shell command
    AtF index -> At index

data TypeError = TypeMismatch (TaggedSelector, ChunkType) (TaggedSelector, ChunkType)
  deriving stock (Show)

type TypeErrorReport = D.Report Text

typecheckSelector :: TaggedSelector -> Either TypeErrorReport (ChunkType {- input -}, ChunkType {- output -})
typecheckSelector =
  CF.unwrap >>> \case
    ComposeF selectors -> do
      selectorTypes <- traverse typecheckSelector selectors
      (snd <$> foldrM1 checkTypes (NE.zip selectors selectorTypes))
    SplitFieldsF _ -> pure (TextType, TextType)
    SplitLinesF -> pure (TextType, TextType)
    SplitWordsF -> pure (TextType, TextType)
    RegexF _ -> pure (TextType, TextType)
    ListOfF ast -> do
      (input, output) <- typecheckSelector ast
      pure (input, ListType output)
    ShellF _ -> pure (TextType, TextType)
    AtF _ ->
      -- TODO: Guess I have to do proper unification here :P
      pure (ListType AnyType, AnyType)
  where
    checkTypes :: (TaggedSelector, (ChunkType, ChunkType)) -> (TaggedSelector, (ChunkType, ChunkType)) -> Either TypeErrorReport (TaggedSelector, (ChunkType, ChunkType))
    checkTypes (sl, (i, l)) (sr, (r, o)) =
      if l `unifies` r
        then
          -- Keep the left selector in the list because we're doing a foldr
          Right (sl, (i, o))
        else Left $ typeErrorReport $ TypeMismatch (sl, l) (sr, r)

untagSelector :: TaggedSelector -> Selector
untagSelector = cata \(_ CofreeF.:< f) -> embed f

typeErrorReport :: TypeError -> D.Report Text
typeErrorReport = \case
  TypeMismatch (lPos CF.:< _, ltyp) (rPos CF.:< _, rtyp) ->
    Diagnose.Err
      Nothing
      "Type error"
      [ (lPos, D.This $ "this selector outputs the type: " <> renderType ltyp),
        (rPos, D.Where $ "but this selector accepts the type: " <> renderType rtyp)
      ]
      []
