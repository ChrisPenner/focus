module Focus.Typechecker (typecheckSelector) where

import Control.Lens
import Control.Monad.Except (ExceptT, runExceptT, withExceptT)
import Control.Monad.State (MonadState (..), StateT, evalStateT, mapStateT)
import Control.Monad.Trans (lift)
import Control.Unification qualified as Unify
import Control.Unification.STVar qualified as Unify
import Control.Unification.Types (UTerm (..))
import Data.Functor.Fixedpoint (Fix)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Text (Text)
import Error.Diagnose qualified as D
import Error.Diagnose qualified as Diagnose
import Focus.Prelude
import Focus.Tagged (Tagged (..))
import Focus.Typechecker.Types (renderType)
import Focus.Typechecker.Types qualified as T
import Focus.Types
import Focus.Untyped (BindingName (..), BindingString (..))
import Focus.Untyped qualified as UT

unificationErrorReport :: TypecheckFailure s -> D.Report Text
unificationErrorReport = \case
  OccursFailure _ trm ->
    case trm of
      UTerm trm' ->
        Diagnose.Err
          Nothing
          "Type error"
          [ (tag trm', D.This $ "Cyclic type detected. Please report this issue." <> renderTyp trm)
          ]
          []
      _ -> error "OccursFailure: Please report this issue."
  MismatchFailure l r ->
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

type UBindings s = M.Map Text (Typ s)

type UnifyM s = StateT (UBindings s) (ExceptT (TypecheckFailure s) (Unify.STBinding s))

type UnifyME e s = StateT (UBindings s) (ExceptT e (Unify.STBinding s))

data TypecheckFailure s
  = OccursFailure (UVar s) (Typ s)
  | MismatchFailure (ChunkTypeT Diagnose.Position (Typ s)) (ChunkTypeT Diagnose.Position (Typ s))

instance Unify.Fallible (ChunkTypeT D.Position) (UVar s) (TypecheckFailure s) where
  occursFailure = OccursFailure
  mismatchFailure = MismatchFailure

liftUnify :: (ExceptT (TypecheckFailure s) (Unify.STBinding s)) a -> UnifyM s a
liftUnify = lift

declareBindings :: BindingDeclarations -> UnifyM s ()
declareBindings bd = do
  ifor_ bd $ \name (pos, typ) -> do
    v <- getOrInitBinding name
    r <- liftUnify $ Unify.unify v (chunkTypeToChunkTypeT pos typ)
    bindings <- get
    put $ M.insert name r bindings
  where
    chunkTypeToChunkTypeT :: D.Position -> ChunkType -> Typ s
    chunkTypeToChunkTypeT pos = \case
      T.TextType -> T.textType pos
      T.ListType t -> T.listType pos (chunkTypeToChunkTypeT pos t)
      T.NumberType -> T.numberType pos
      T.RegexMatchType -> T.regexMatchType pos

getOrInitBinding :: Text -> UnifyM s (Typ s)
getOrInitBinding name = do
  bindings <- get
  case M.lookup name bindings of
    Just v -> pure v
    Nothing -> do
      v <- lift . lift $ (UVar <$> Unify.freeVar)
      put $ M.insert name v bindings
      pure v

typecheckSelector :: UT.TaggedSelector -> Either TypeErrorReport ()
typecheckSelector t =
  void $ Unify.runSTBinding $ runExceptT $ flip evalStateT mempty $ runUnification t
  where
    runUnification :: forall s. UT.TaggedSelector -> UnifyME TypeErrorReport s (Fix (ChunkTypeT D.Position))
    runUnification taggedSelector = mapStateT (withExceptT unificationErrorReport) $ do
      typ <- go taggedSelector
      trm <- liftUnify $ Unify.applyBindings (UTerm typ)
      case Unify.freeze trm of
        Nothing -> pure $ error "Freeze failed"
        Just r -> pure r

    go :: UT.TaggedSelector -> UnifyM s (ChunkTypeT D.Position (Typ s))
    go = \case
      UT.Compose _pos (s NE.:| rest) -> do
        s' <- go s
        foldlM compose s' rest
      UT.SplitFields pos _delim -> pure $ T.Arrow pos (T.textType pos) (T.textType pos)
      UT.SplitLines pos -> do
        let typ = T.Arrow pos (T.textType pos) (T.textType pos)
        pure (typ)
      UT.SplitWords pos -> do
        let typ = T.Arrow pos (T.textType pos) (T.textType pos)
        pure (typ)
      UT.Regex pos _regex bindings -> do
        declareBindings bindings
        pure $ T.Arrow pos (T.textType pos) (T.textType pos)
      UT.RegexMatches pos -> pure $ T.Arrow pos (T.regexMatchType pos) (T.textType pos)
      UT.RegexGroups pos _pat bindings -> do
        declareBindings bindings
        pure $ T.Arrow pos (T.textType pos) (T.textType pos)
      UT.ListOf pos inner -> do
        (innerTyp) <- go inner
        case innerTyp of
          T.Arrow _ inp out -> do
            pure $ T.Arrow pos inp (T.listType pos out)
          _ -> error "ListOf: Expected Arrow"
      UT.Filter _pos inner -> go inner
      UT.Not _pos inner -> go inner
      UT.Splat pos -> do
        inp <- freshVar
        pure $ T.Arrow pos (T.listType pos inp) inp
      UT.Shell pos script shellMode -> do
        unifyBindingString script
        inp <- case shellMode of
          Normal -> pure $ T.textType pos
          NullStdin -> freshVar
        pure $ T.Arrow pos inp (T.textType pos)
      UT.At pos _n -> do
        inp <- freshVar
        pure $ T.Arrow pos (T.listType pos inp) inp
      UT.Take _pos _n inner -> do go inner
      UT.TakeEnd _pos _n inner -> do go inner
      UT.Drop _pos _n inner -> do go inner
      UT.DropEnd _pos _n inner -> do go inner
      UT.Contains pos _txt -> do
        pure $ T.Arrow pos (T.textType pos) (T.textType pos)

    compose ::
      forall s.
      (ChunkTypeT Diagnose.Position (Typ s)) ->
      UT.TaggedSelector ->
      UnifyM s (ChunkTypeT Diagnose.Position (Typ s))
    compose l rTagged = do
      case l of
        (T.Arrow lpos li lm) -> do
          go rTagged >>= \case
            (T.Arrow rpos rm ro) -> do
              _ <- liftUnify $ Unify.unify lm rm
              let typ = T.Arrow (lpos <> rpos) li ro
              pure typ
            _ -> error "Expected Arrow type in compose"
        _ -> error "Expected Arrow type in compose"

unifyBindingString :: BindingString -> UnifyM s ()
unifyBindingString (BindingString bindings) = do
  for_ bindings $ \case
    Left (BindingName name, pos) -> do
      v <- getOrInitBinding name
      _ <- liftUnify $ Unify.unify v (T.textType pos)
      pure ()
    Right _ -> pure ()

freshVar :: UnifyM s (Typ s)
freshVar = (Unify.UVar <$> (lift . lift $ Unify.freeVar))
