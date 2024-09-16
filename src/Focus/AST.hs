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
import Control.Comonad (Comonad (..))
import Control.Comonad.Cofree (Cofree)
import Control.Comonad.Cofree qualified as CF
import Control.Comonad.Trans.Cofree qualified as CofreeF
import Data.Foldable (foldlM)
import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Type.Equality (TestEquality (..), (:~:) (Refl))
import Error.Diagnose qualified as D
import Error.Diagnose qualified as Diagnose
import Focus.Prelude ()
import Focus.Typechecker.Types (SomeTypedSelector (..), inputType, outputType)
import Focus.Typechecker.Types qualified as Typechecked
import Focus.Types (ChunkType (..), getChunkType, renderType)
import Text.Regex.PCRE.Heavy (Regex)
import Text.Show.Deriving (deriveShow1)

type TaggedSelector = Cofree SelectorF D.Position

data SelectorF r
  = ComposeF (NonEmpty r)
  | SplitFieldsF Text {- delimeter -}
  | SplitLinesF
  | SplitWordsF
  | RegexF Regex {- pattern -}
  | RegexMatchesF
  | RegexGroupsF
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
  | RegexMatches
  | RegexGroups
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
    RegexMatches -> RegexMatchesF
    RegexGroups -> RegexGroupsF
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
    RegexMatchesF -> RegexMatches
    RegexGroupsF -> RegexGroups
    ListOfF ast -> ListOf ast
    ShellF command -> Shell command
    AtF index -> At index

data TypeError
  = TypeMismatch (D.Position, ChunkType) (D.Position, ChunkType)
  deriving stock (Show)

type TypeErrorReport = D.Report Text

-- oldTypecheckSelector :: TaggedSelector -> Either TypeErrorReport (ChunkType {- input -}, ChunkType {- output -})
-- oldTypecheckSelector =
--   CF.unwrap >>> \case
--     ComposeF selectors -> do
--       selectorTypes <- traverse typecheckSelector selectors
--       (snd <$> foldrM1 checkTypes (NE.zip selectors selectorTypes))
--     SplitFieldsF _ -> pure (TextType, TextType)
--     SplitLinesF -> pure (TextType, TextType)
--     SplitWordsF -> pure (TextType, TextType)
--     RegexF _ -> pure (TextType, RegexMatchType)
--     RegexMatchesF -> pure (RegexMatchType, TextType)
--     RegexGroupsF -> pure (RegexMatchType, TextType)
--     ListOfF ast -> do
--       (input, output) <- typecheckSelector ast
--       pure (input, ListType output)
--     ShellF _ -> pure (TextType, TextType)
--     AtF _ ->
--       -- TODO: Guess I have to do proper unification here :P
--       pure (ListType AnyType, AnyType)
--   where
--     checkTypes :: (TaggedSelector, (ChunkType, ChunkType)) -> (TaggedSelector, (ChunkType, ChunkType)) -> Either TypeErrorReport (TaggedSelector, (ChunkType, ChunkType))
--     checkTypes (sl, (i, l)) (sr, (r, o)) =
--       if l `unifies` r
--         then
--           -- Keep the left selector in the list because we're doing a foldr
--           Right (sl, (i, o))
--         else Left $ typeErrorReport $ TypeMismatch (sl, l) (sr, r)

untagSelector :: TaggedSelector -> Selector
untagSelector = cata \(_ CofreeF.:< f) -> embed f

typeErrorReport :: TypeError -> D.Report Text
typeErrorReport = \case
  TypeMismatch (lPos, ltyp) (rPos, rtyp) ->
    Diagnose.Err
      Nothing
      "Type error"
      [ (lPos, D.This $ "this selector outputs the type: " <> renderType ltyp),
        (rPos, D.Where $ "but this selector accepts the type: " <> renderType rtyp)
      ]
      []

typecheckSelector :: TaggedSelector -> Either TypeErrorReport SomeTypedSelector
typecheckSelector =
  CF.unwrap >>> \case
    ComposeF (s NE.:| rest) -> do
      s' <- typecheckSelector s
      snd <$> foldlM compose (extract s, s') rest
    SplitFieldsF delim -> pure $ SomeTypedSelector $ Typechecked.SplitFields delim
    SplitLinesF -> pure $ SomeTypedSelector Typechecked.SplitLines
    SplitWordsF -> pure $ SomeTypedSelector Typechecked.SplitWords
    RegexF pat -> pure $ SomeTypedSelector $ Typechecked.Regex pat
    RegexMatchesF -> pure $ SomeTypedSelector Typechecked.RegexMatches
    RegexGroupsF -> pure $ SomeTypedSelector Typechecked.RegexGroups
    ListOfF inner -> do
      typecheckSelector inner >>= \case
        SomeTypedSelector inner' ->
          pure $ SomeTypedSelector $ Typechecked.ListOf inner'
    ShellF script -> pure $ SomeTypedSelector $ Typechecked.Shell script
    AtF n -> pure $ SomeTypedSelector $ Typechecked.At n
  where
    compose :: (D.Position, SomeTypedSelector) -> TaggedSelector -> Either TypeErrorReport (D.Position, SomeTypedSelector)
    compose (lPos, l) r = do
      let rPos = extract r
      typecheckSelector r >>= \case
        SomeTypedSelector r' ->
          case l of
            SomeTypedSelector l' ->
              case testEquality (outputType l') (inputType r') of
                Just Refl -> Right $ (lPos <> rPos, SomeTypedSelector $ Typechecked.Compose l' r')
                Nothing -> Left $ typeErrorReport $ TypeMismatch (lPos, getChunkType $ outputType l') (rPos, getChunkType $ inputType r')
