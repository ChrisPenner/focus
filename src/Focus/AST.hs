{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Focus.AST
  ( Selector (..),
    TaggedSelector,
    TypeError (..),
    TypeErrorReport,
    typecheckSelector,
    typecheckSelectorUnified,
  )
where

import Control.Monad.Except (ExceptT, runExceptT, withExceptT)
import Control.Monad.Trans (lift)
import Control.Unification qualified as Unify
import Control.Unification.STVar qualified as Unify
import Control.Unification.Types (UFailure, UTerm (..))
import Control.Unification.Types qualified as Unify
import Data.Foldable (foldlM)
import Data.Functor.Fixedpoint (Fix)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Type.Equality (TestEquality (..), (:~:) (Refl))
import Error.Diagnose qualified as D
import Error.Diagnose qualified as Diagnose
import Focus.Prelude
import Focus.Tagged (Tagged (..))
import Focus.Typechecker.Types (ChunkType, ChunkTypeT, SomeTypedSelector (..), Typ, UVar, getChunkType, inputType, outputType, renderType)
import Focus.Typechecker.Types qualified as T
import Focus.Typechecker.Types qualified as Typechecked
import Text.Regex.PCRE.Heavy (Regex)

type TaggedSelector = Selector D.Position

data Selector a
  = Compose a (NonEmpty (Selector a))
  | SplitFields a Text {- delimeter -}
  | SplitLines a
  | SplitWords a
  | Regex a Regex
  | RegexMatches a
  | RegexGroups a
  | ListOf a (Selector a)
  | Shell a Text
  | At a Int
  deriving stock (Show, Functor, Foldable, Traversable)

instance Tagged (Selector a) a where
  tag = \case
    Compose a _ -> a
    SplitFields a _ -> a
    SplitLines a -> a
    SplitWords a -> a
    Regex a _ -> a
    RegexMatches a -> a
    RegexGroups a -> a
    ListOf a _ -> a
    Shell a _ -> a
    At a _ -> a

data TypeError
  = TypeMismatch (D.Position, ChunkType) (D.Position, ChunkType)
  deriving stock (Show)

type TypeErrorReport = D.Report Text

-- oldTypecheckSelector :: TaggedSelector -> Either TypeErrorReport (ChunkType {- input -}, ChunkType {- output -})
-- oldTypecheckSelector =
--   CF.unwrap >>> \case
--     Compose selectors -> do
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

unificationErrorReport :: UnifyFailure s -> D.Report Text
unificationErrorReport = \case
  Unify.OccursFailure _ trm ->
    case trm of
      UTerm trm' ->
        Diagnose.Err
          Nothing
          "Type error"
          [ (tag trm', D.This $ "Cyclic type detected. Please report this issue." <> renderTyp trm)
          ]
          []
      _ -> error "OccursFailure: Please report this issue."
  Unify.MismatchFailure l r ->
    let lPos = tag l
        rPos = tag r
     in Diagnose.Err
          Nothing
          "Type error"
          [ (lPos, D.This $ "this selector outputs the type: " <> renderUTyp l),
            (rPos, D.Where $ "but this selector accepts the type: " <> renderUTyp r)
          ]
          []
  where
    renderTyp :: Typ s -> Text
    renderTyp = \case
      UVar v -> tShow v
      UTerm t -> renderUTyp t
    renderUTyp :: ChunkTypeT D.Position (Typ s) -> Text
    renderUTyp = \case
      T.Arrow _ a b -> "(" <> renderTyp a <> " -> " <> renderTyp b <> ")"
      T.TextTypeT {} -> renderType T.TextType
      T.ListTypeT _ t -> "[" <> renderTyp t <> "]"
      T.NumberTypeT _ -> renderType T.NumberType
      T.RegexMatchTypeT _ -> renderType T.RegexMatchType

typecheckSelector :: TaggedSelector -> Either TypeErrorReport (SomeTypedSelector D.Position)
typecheckSelector = \case
  Compose _pos (s NE.:| rest) -> do
    s' <- typecheckSelector s
    foldlM compose s' rest
  SplitFields pos delim -> pure $ SomeTypedSelector $ Typechecked.SplitFields pos delim
  SplitLines pos -> pure $ SomeTypedSelector $ Typechecked.SplitLines pos
  SplitWords pos -> pure $ SomeTypedSelector $ Typechecked.SplitWords pos
  Regex pos pat -> pure $ SomeTypedSelector $ Typechecked.Regex pos pat
  RegexMatches pos -> pure $ SomeTypedSelector $ Typechecked.RegexMatches pos
  RegexGroups pos -> pure $ SomeTypedSelector $ Typechecked.RegexGroups pos
  ListOf pos inner -> do
    typecheckSelector inner >>= \case
      SomeTypedSelector inner' ->
        pure $ SomeTypedSelector $ Typechecked.ListOf pos inner'
  Shell pos script -> pure $ SomeTypedSelector $ Typechecked.Shell pos script
  At pos n -> pure $ SomeTypedSelector $ Typechecked.At pos n
  where
    compose :: SomeTypedSelector D.Position -> TaggedSelector -> Either TypeErrorReport (SomeTypedSelector D.Position)
    compose l r = do
      typecheckSelector r >>= \case
        SomeTypedSelector r' ->
          case l of
            SomeTypedSelector l' ->
              case testEquality (outputType l') (inputType r') of
                Just Refl -> Right $ (SomeTypedSelector $ Typechecked.Compose (tag l' <> tag r) l' r')
                Nothing -> Left $ typeErrorReport $ TypeMismatch (tag l', getChunkType $ outputType l') (tag r', getChunkType $ inputType r')

type UnifyM e s = ExceptT e (Unify.STBinding s)

type UnifyFailure s = UFailure (ChunkTypeT D.Position) (UVar s)

typecheckSelectorUnified :: TaggedSelector -> Either TypeErrorReport (Fix (ChunkTypeT D.Position))
typecheckSelectorUnified t =
  Unify.runSTBinding $ runExceptT $ runUnification t
  where
    runUnification :: forall s. TaggedSelector -> UnifyM TypeErrorReport s (Fix (ChunkTypeT D.Position))
    runUnification ts = withExceptT unificationErrorReport $ do
      typ <- go ts
      trm <- Unify.applyBindings (UTerm typ)
      case Unify.freeze trm of
        Nothing -> pure $ error "Freeze failed"
        Just r -> pure r

    go :: TaggedSelector -> UnifyM (UnifyFailure s) s (ChunkTypeT D.Position (Typ s))
    go ts =
      let pos = tag ts
       in case ts of
            Compose _pos (s NE.:| rest) -> do
              s' <- go s
              foldlM compose s' rest
            SplitFields {} -> pure $ T.Arrow pos (T.textType pos) (T.textType pos)
            SplitLines {} -> pure $ T.Arrow pos (T.textType pos) (T.textType pos)
            SplitWords {} -> pure $ T.Arrow pos (T.textType pos) (T.textType pos)
            Regex {} -> pure $ T.Arrow pos (T.textType pos) (T.regexMatchType pos)
            RegexMatches {} -> pure $ T.Arrow pos (T.regexMatchType pos) (T.textType pos)
            RegexGroups {} -> pure $ T.Arrow pos (T.regexMatchType pos) (T.textType pos)
            ListOf _ inner -> do
              innerT <- go inner
              inp <- freshVar
              out <- freshVar
              _ <- Unify.unify (T.arrow pos inp out) (UTerm innerT)
              pure $ T.Arrow pos inp (T.listType pos out)
            Shell {} -> pure $ T.Arrow pos (T.textType pos) (T.textType pos)
            At {} -> do
              inp <- freshVar
              pure $ T.Arrow pos (T.listType pos inp) inp

    compose :: forall s. ChunkTypeT Diagnose.Position (Typ s) -> TaggedSelector -> UnifyM (UnifyFailure s) s (ChunkTypeT Diagnose.Position (Typ s))
    compose lt r = do
      rt <- go r
      (li, m, ro) <- lift $ (,,) <$> Unify.freeVar <*> Unify.freeVar <*> Unify.freeVar
      let posL = tag lt
      let posR = tag rt
      _ <- Unify.unify (T.arrow posL (Unify.UVar li) (Unify.UVar m)) (UTerm lt)
      _ <- Unify.unify (T.arrow posR (Unify.UVar m) (Unify.UVar ro)) (UTerm rt)
      pure (T.Arrow (posL <> posR) (Unify.UVar li) (Unify.UVar ro))

freshVar :: UnifyM (UnifyFailure s) s (Typ s)
freshVar = lift $ Unify.UVar <$> Unify.freeVar

-- ListOf pos inner -> do
--   typecheckSelector inner >>= \case
--     SomeTypedSelector inner' ->
--       pure $ SomeTypedSelector $ Typechecked.ListOf pos inner'
-- Shell pos script -> pure $ SomeTypedSelector $ Typechecked.Shell pos script
-- At pos n -> pure $ SomeTypedSelector $ Typechecked.At pos n
