module Focus.Typechecker (typecheckSelector) where

import Control.Monad.Except (ExceptT, runExceptT, withExceptT)
import Control.Monad.Trans (lift)
import Control.Unification qualified as Unify
import Control.Unification.STVar qualified as Unify
import Control.Unification.Types (UFailure, UTerm (..))
import Control.Unification.Types qualified as Unify
import Data.Functor.Fixedpoint (Fix)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Error.Diagnose qualified as D
import Error.Diagnose qualified as Diagnose
import Focus.AST (Selector (..), TaggedSelector)
import Focus.Prelude
import Focus.Tagged (Tagged (..))
import Focus.Typechecker.Types (ChunkTypeT, SomeTypedSelector (..), Typ, UVar, renderType)
import Focus.Typechecker.Types qualified as T
import Focus.Typechecker.Types qualified as Typechecked
import Unsafe.Coerce (unsafeCoerce)

type TypeErrorReport = D.Report Text

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
          [ (lPos, D.This $ "this selector outputs: " <> renderUTyp l),
            (rPos, D.Where $ "but this selector expects: " <> renderUTyp r)
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

type UnifyM e s = ExceptT e (Unify.STBinding s)

type UnifyFailure s = UFailure (ChunkTypeT D.Position) (UVar s)

typecheckSelector :: TaggedSelector -> Either TypeErrorReport (SomeTypedSelector D.Position)
typecheckSelector t =
  fmap snd $ Unify.runSTBinding $ runExceptT $ runUnification t
  where
    runUnification :: forall s. TaggedSelector -> UnifyM TypeErrorReport s (Fix (ChunkTypeT D.Position), SomeTypedSelector D.Position)
    runUnification taggedSelector = withExceptT unificationErrorReport $ do
      (typ, typedSelector) <- go taggedSelector
      trm <- Unify.applyBindings (UTerm typ)
      case Unify.freeze trm of
        Nothing -> pure $ error "Freeze failed"
        Just r -> pure (r, typedSelector)

    go :: TaggedSelector -> UnifyM (UnifyFailure s) s (ChunkTypeT D.Position (Typ s), SomeTypedSelector D.Position)
    go = \case
      Compose _pos (s NE.:| rest) -> do
        s' <- go s
        foldlM compose s' rest
      SplitFields pos delim -> do
        let typ = T.Arrow pos (T.textType pos) (T.textType pos)
        let ts = SomeTypedSelector $ Typechecked.SplitFields pos delim
        pure (typ, ts)
      SplitLines pos -> do
        let typ = T.Arrow pos (T.textType pos) (T.textType pos)
        let ts = SomeTypedSelector $ Typechecked.SplitLines pos
        pure (typ, ts)
      SplitWords pos -> do
        let typ = T.Arrow pos (T.textType pos) (T.textType pos)
        let ts = SomeTypedSelector $ Typechecked.SplitWords pos
        pure (typ, ts)
      Regex pos re -> do
        let typ = T.Arrow pos (T.textType pos) (T.regexMatchType pos)
        let ts = SomeTypedSelector $ Typechecked.Regex pos re
        pure (typ, ts)
      RegexMatches pos -> do
        let typ = T.Arrow pos (T.regexMatchType pos) (T.textType pos)
        let ts = SomeTypedSelector $ Typechecked.RegexMatches pos
        pure (typ, ts)
      RegexGroups pos -> do
        let typ = T.Arrow pos (T.regexMatchType pos) (T.listType pos (T.textType pos))
        let ts = SomeTypedSelector $ Typechecked.RegexGroups pos
        pure (typ, ts)
      ListOf pos inner -> do
        (innerTyp, innerTS) <- go inner
        case innerTyp of
          T.Arrow _ inp out -> do
            let typ = T.Arrow pos inp (T.listType pos out)
            case innerTS of
              SomeTypedSelector inner' -> do
                pure $ (typ, SomeTypedSelector $ Typechecked.ListOf pos inner')
          _ -> error "ListOf: Expected Arrow"
      FilterBy pos inner -> do
        (innerTyp, innerTS) <- go inner
        case innerTS of
          SomeTypedSelector inner' -> do
            pure $ (innerTyp, SomeTypedSelector $ Typechecked.FilterBy pos inner')
      Splat pos -> do
        inp <- freshVar
        let typ = T.Arrow pos (T.listType pos inp) inp
        let ts = SomeTypedSelector $ Typechecked.Splat pos
        pure (typ, ts)
      Shell pos script ->
        let typ = T.Arrow pos (T.textType pos) (T.textType pos)
            ts = SomeTypedSelector $ Typechecked.Shell pos script
         in pure (typ, ts)
      At pos n -> do
        inp <- freshVar
        let typ = T.Arrow pos (T.listType pos inp) inp
        let ts = SomeTypedSelector $ Typechecked.At pos n
        pure (typ, ts)

    compose ::
      forall s.
      (ChunkTypeT Diagnose.Position (Typ s), SomeTypedSelector Diagnose.Position) ->
      TaggedSelector ->
      UnifyM (UnifyFailure s) s (ChunkTypeT Diagnose.Position (Typ s), SomeTypedSelector Diagnose.Position)
    compose l rTagged = do
      case l of
        (T.Arrow lpos li lm, lTS) -> do
          go rTagged >>= \case
            (T.Arrow rpos rm ro, rTS) -> do
              _ <- Unify.unify lm rm
              let typ = T.Arrow (lpos <> rpos) li ro
              pure (typ, coerceCompose lTS rTS)
            _ -> error "Expected Arrow type in compose"
        _ -> error "Expected Arrow type in compose"
    -- Use unsafeCoerce in this one place so we can be typesafe everywhere else.
    -- This is safe because we'll just fail typechecking if the types don't match.
    coerceCompose :: SomeTypedSelector Diagnose.Position -> SomeTypedSelector Diagnose.Position -> SomeTypedSelector Diagnose.Position
    coerceCompose (SomeTypedSelector l) (SomeTypedSelector r) = SomeTypedSelector $ Typechecked.Compose (tag l <> tag r) l (unsafeCoerce r)

freshVar :: UnifyM (UnifyFailure s) s (Typ s)
freshVar = lift $ Unify.UVar <$> Unify.freeVar
