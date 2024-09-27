module Focus.Typechecker
  ( typecheckSelector,
    typecheckModify,
    typecheckAction,
  )
where

import Control.Lens
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT, withExceptT)
import Control.Monad.State (MonadState (..), StateT, evalStateT, mapStateT)
import Control.Monad.Trans (lift)
import Control.Unification qualified as Unify
import Control.Unification.STVar qualified as Unify
import Control.Unification.Types (UTerm (..))
import Data.Foldable1 qualified as F1
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
import Focus.Untyped (BindingName (..), BindingString (..), Expr (..), absurdF)
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
  UndeclaredBinding pos name ->
    Diagnose.Err
      Nothing
      "Unknown binding"
      [(pos, D.This $ "Binding " <> tShow name <> " is not in scope.")]
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
  | UndeclaredBinding Diagnose.Position Text

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

expectBinding :: Diagnose.Position -> Text -> UnifyM s (Typ s)
expectBinding pos name = do
  bindings <- get
  case M.lookup name bindings of
    Just v -> pure v
    Nothing -> throwError $ UndeclaredBinding pos name

typecheckModify :: UT.TaggedSelector -> UT.Selector Expr D.Position -> Either TypeErrorReport ()
typecheckModify selector expr = do
  typecheckThing unifyModify (selector, expr)

typecheckAction :: UT.TaggedAction -> Either TypeErrorReport ()
typecheckAction = typecheckThing unifyAction

unifyModify :: (UT.TaggedSelector, UT.Selector Expr D.Position) -> UnifyME (TypecheckFailure s) s (Typ s, Typ s)
unifyModify (selector, expr) = do
  (selectorIn, selectorOut) <- unifySelector selector
  (exprIn, exprOut) <- unifyAction expr
  _ <- liftUnify $ Unify.unify selectorOut exprIn
  _ <- liftUnify $ Unify.unify selectorOut exprOut
  pure $ (selectorIn, exprOut)

typecheckSelector :: UT.TaggedSelector -> Either TypeErrorReport ()
typecheckSelector = typecheckThing unifySelector

typecheckThing :: forall thing. (forall s. thing -> UnifyME (TypecheckFailure s) s (Typ s, Typ s)) -> thing -> Either TypeErrorReport ()
typecheckThing typechecker t =
  void $ Unify.runSTBinding $ runExceptT $ flip evalStateT mempty $ mapStateT (withExceptT unificationErrorReport) $ void $ typechecker t

-- where
--   go :: thing -> StateT (UBindings s) (ExceptT (TypecheckFailure s) (Unify.STBinding s)) (Fix (ChunkTypeT D.Position))
--   go thing = do
--     typ <- typechecker thing
--     trm <- liftUnify $ Unify.applyBindings (UTerm typ)
--     case Unify.freeze trm of
--       Nothing -> pure $ error "Freeze failed"
--       Just r -> pure r

unifySelector :: UT.TaggedSelector -> UnifyM s (Typ s, Typ s)
unifySelector = unifySelectorG absurdF

unifyAction :: UT.TaggedAction -> UnifyM s (Typ s, Typ s)
unifyAction = unifySelectorG unifyExpr

unifySelectorG :: forall s expr. (expr D.Position -> UnifyM s (Typ s, Typ s)) -> UT.Selector expr D.Position -> UnifyM s (Typ s, Typ s)
unifySelectorG goExpr = \case
  UT.Compose _pos (s NE.:| rest) -> do
    s' <- unifySelectorG goExpr s
    foldlM compose s' rest
  UT.SplitFields pos _delim -> pure $ (T.textType pos, T.textType pos)
  UT.SplitLines pos -> do
    pure (T.textType pos, T.textType pos)
  UT.SplitWords pos -> do
    pure (T.textType pos, T.textType pos)
  UT.Regex pos _regex bindings -> do
    declareBindings bindings
    pure $ (T.textType pos, T.textType pos)
  UT.RegexMatches pos -> pure $ (T.regexMatchType pos, T.textType pos)
  UT.RegexGroups pos _pat bindings -> do
    declareBindings bindings
    pure (T.textType pos, T.textType pos)
  UT.ListOf pos inner -> do
    (inp, out) <- unifySelectorG goExpr inner
    pure (inp, T.listType pos out)
  UT.Filter _pos inner -> unifySelectorG goExpr inner
  UT.Not _pos inner -> unifySelectorG goExpr inner
  UT.Splat pos -> do
    inp <- freshVar
    pure $ (T.listType pos inp, inp)
  UT.Shell pos script shellMode -> do
    inp <- case shellMode of
      Normal -> pure $ T.textType pos
      NullStdin -> freshVar
    unifyBindingString inp script
    pure $ (inp, T.textType pos)
  UT.At pos _n -> do
    inp <- freshVar
    pure (T.listType pos inp, inp)
  UT.Take _pos _n inner -> do unifySelectorG goExpr inner
  UT.TakeEnd _pos _n inner -> do unifySelectorG goExpr inner
  UT.Drop _pos _n inner -> do unifySelectorG goExpr inner
  UT.DropEnd _pos _n inner -> do unifySelectorG goExpr inner
  UT.Contains pos _txt -> do
    pure $ (T.textType pos, T.textType pos)
  UT.Action _pos expr -> do
    goExpr expr
  where
    compose ::
      (Typ s, Typ s) ->
      UT.Selector expr Diagnose.Position ->
      UnifyM s (Typ s, Typ s)
    compose (li, lm) rTagged = do
      (rm, ro) <- unifySelectorG goExpr rTagged
      _ <- liftUnify $ Unify.unify lm rm
      pure (li, ro)

unifyExpr :: UT.TaggedExpr -> UnifyME (TypecheckFailure s) s (Typ s, Typ s)
unifyExpr = \case
  Binding _pos InputBinding -> do
    inputTyp <- freshVar
    pure (inputTyp, inputTyp)
  Binding pos (BindingName name) -> do
    bindingTyp <- expectBinding pos name
    -- any input type, output type matches the binding
    inputTyp <- freshVar
    pure (inputTyp, bindingTyp)
  Str pos bindingStr -> do
    inputTyp <- freshVar
    unifyBindingString inputTyp bindingStr
    pure $ (inputTyp, (T.textType pos))
  Number pos _num -> do
    inputTyp <- freshVar
    pure $ (inputTyp, (T.numberType pos))
  StrConcat pos innerExpr -> do
    (inp, innerResult) <- unifyAction innerExpr
    _ <- liftUnify $ Unify.unify innerResult (T.listType pos (T.textType pos))
    pure $ (inp, (T.textType pos))
  Intersperse _pos actions -> do
    typs <- for actions unifyAction
    liftUnify $ F1.foldrM1 (\(i1, o1) (i2, o2) -> (,) <$> Unify.unify i1 i2 <*> Unify.unify o1 o2) typs

unifyBindingString :: Typ s -> BindingString -> UnifyM s ()
unifyBindingString inputTyp (BindingString bindings) = do
  for_ bindings $ \case
    Left (BindingName name, pos) -> do
      v <- expectBinding pos name
      _ <- liftUnify $ Unify.unify v (T.textType pos)
      pure ()
    Left (InputBinding, pos) -> do
      _ <- liftUnify $ Unify.unify inputTyp (T.textType pos)
      pure ()
    Right _ -> pure ()

freshVar :: UnifyM s (Typ s)
freshVar = (Unify.UVar <$> (lift . lift $ Unify.freeVar))
