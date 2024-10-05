module Focus.Typechecker
  ( typecheckSelector,
    typecheckModify,
    typecheckView,
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
import Data.Set.NonEmpty (NESet)
import Data.Text (Text)
import Data.Text qualified as Text
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
          ( toList (tag trm') <&> \pos ->
              (pos, D.This $ "Cyclic type detected. Please report this issue." <> renderTyp trm)
          )
          []
      _ -> error "OccursFailure: Please report this issue."
  MismatchFailure l r ->
    let lPos = tag l
        rPos = tag r
     in Diagnose.Err
          Nothing
          "Type error"
          ( expandPositions D.This ("this selector outputs: " <> renderUTyp l) lPos
              <> expandPositions D.This ("but this selector expects: " <> renderUTyp r) rPos
          )
          []
  UndeclaredBinding pos name ->
    Diagnose.Err
      Nothing
      "Unknown binding"
      [(pos, D.This $ "Binding " <> tShow name <> " is not in scope.")]
      []
  ExpectedSingularArity pos arity ->
    Diagnose.Err
      Nothing
      "Multiplicity error"
      [(pos, D.This $ "Actions must result in exactly one result." <> tShow arity)]
      []
  NonTextInput pos typ ->
    Diagnose.Err
      Nothing
      "Type error"
      (expandPositions D.This ("Selector must accept text input, but instead expects: " <> renderTyp typ) pos)
      []
  ExprInSelector pos ->
    Diagnose.Err
      Nothing
      "Type error"
      (expandPositions D.This "Expressions are not valid where a selector was expected." pos)
      []
  where
    varNames :: [Text]
    varNames =
      let letters = (Text.singleton <$> ['a' .. 'z'])
       in letters <> zipWith (<>) (cycle letters) (Text.pack . show <$> [(0 :: Int) ..])
    renderTyp :: Typ s -> Text
    renderTyp = \case
      UVar v ->
        varNames !! succ (maxBound + Unify.getVarID v)
      UTerm t -> renderUTyp t
    renderUTyp :: ChunkTypeT (NESet D.Position) (Typ s) -> Text
    renderUTyp = \case
      T.CastableTypeT _ typ -> "!" <> renderTyp typ
      T.Arrow _ a b -> "(" <> renderTyp a <> " -> " <> renderTyp b <> ")"
      T.TextTypeT {} -> renderType T.TextType
      T.ListTypeT _ t -> "[" <> renderTyp t <> "]"
      T.NumberTypeT _ -> renderType T.NumberType
      T.RegexMatchTypeT _ -> renderType T.RegexMatchType
      T.JsonTypeT _ -> renderType T.JsonType

expandPositions :: (Foldable f) => (Text -> Diagnose.Marker Text) -> Text -> f D.Position -> [(D.Position, D.Marker Text)]
expandPositions marker msg xs = toList xs <&> \pos -> (pos, marker msg)

type UBindings s = M.Map Text (Typ s)

type UnifyM s = StateT (UBindings s) (ExceptT (TypecheckFailure s) (Unify.STBinding s))

type UnifyME e s = StateT (UBindings s) (ExceptT e (Unify.STBinding s))

data TypecheckFailure s
  = OccursFailure (UVar s) (Typ s)
  | MismatchFailure (ChunkTypeT (NESet Diagnose.Position) (Typ s)) (ChunkTypeT (NESet Diagnose.Position) (Typ s))
  | UndeclaredBinding Diagnose.Position Text
  | ExpectedSingularArity Diagnose.Position ReturnArity
  | NonTextInput (NESet Diagnose.Position) (Typ s)
  | ExprInSelector (NESet Diagnose.Position)

instance Unify.Fallible (ChunkTypeT (NESet D.Position)) (UVar s) (TypecheckFailure s) where
  occursFailure = OccursFailure
  mismatchFailure = MismatchFailure

liftUnify :: (ExceptT (TypecheckFailure s) (Unify.STBinding s)) a -> UnifyM s a
liftUnify = lift

declareBindings :: BindingDeclarations -> UnifyM s ()
declareBindings bd = do
  ifor_ bd $ \name (pos, typ) -> do
    v <- initBinding name
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
      T.JsonType -> T.jsonType pos

initBinding :: Text -> UnifyM s (Typ s)
initBinding name = do
  bindings <- get
  typ <- freshVar
  put $ M.insert name typ bindings
  pure typ

_getBinding :: Text -> D.Position -> UnifyM s (Typ s)
_getBinding name pos = do
  bindings <- get
  case M.lookup name bindings of
    Just v -> pure v
    Nothing -> throwError $ UndeclaredBinding pos name

expectBinding :: Diagnose.Position -> Text -> UnifyM s (Typ s)
expectBinding pos name = do
  bindings <- get
  case M.lookup name bindings of
    Just v -> pure v
    Nothing -> throwError $ UndeclaredBinding pos name

typecheckModify :: UT.TaggedSelector -> UT.Selector Expr D.Position -> Either TypeErrorReport ()
typecheckModify selector expr = do
  typecheckThing $ do
    (inp, _out, _arity) <- unifyModify (selector, expr)
    expectTextInput inp (tag selector)
    pure ()

typecheckView :: UT.TaggedAction -> Either TypeErrorReport ()
typecheckView action = typecheckThing $ do
  (inp, _out, _arity) <- unifyAction action
  expectTextInput inp (tag action)

expectTextInput :: Typ s -> Diagnose.Position -> UnifyM s ()
expectTextInput inp pos =
  void . liftUnify $ withExceptT rewriteErr $ Unify.unify inp (T.textType pos)
  where
    rewriteErr = \case
      MismatchFailure a _ -> NonTextInput (tag a) (UTerm a)
      x -> x

unifyModify :: (UT.TaggedSelector, UT.Selector Expr D.Position) -> UnifyME (TypecheckFailure s) s (Typ s, Typ s, ReturnArity)
unifyModify (selector, expr) = do
  (selectorIn, selectorOut, _selArity) <- unifySelector selector
  (exprIn, exprOut, actionArity) <- unifyAction expr
  _ <- liftUnify $ Unify.unify selectorOut exprIn
  _ <- liftUnify $ Unify.unify selectorOut exprOut
  -- case actionArity of
  --   Exactly 1 -> pure ()
  --   _ -> throwError $ ExpectedSingularArity (tag expr) actionArity
  pure (selectorIn, exprOut, actionArity)

typecheckSelector :: UT.TaggedSelector -> Either TypeErrorReport ()
typecheckSelector sel = typecheckThing do
  (inp, _out, _arity) <- unifySelector sel
  expectTextInput inp (tag sel)
  pure ()

typecheckThing :: (forall s. UnifyME (TypecheckFailure s) s ()) -> Either TypeErrorReport ()
typecheckThing m = do
  Unify.runSTBinding $ runExceptT $ flip evalStateT mempty $ mapStateT (withExceptT unificationErrorReport) $ void $ m

-- where
--   go :: thing -> StateT (UBindings s) (ExceptT (TypecheckFailure s) (Unify.STBinding s)) (Fix (ChunkTypeT D.Position))
--   go thing = do
--     typ <- typechecker thing
--     trm <- liftUnify $ Unify.applyBindings (UTerm typ)
--     case Unify.freeze trm of
--       Nothing -> pure $ error "Freeze failed"
--       Just r -> pure r

unifySelector :: UT.TaggedSelector -> UnifyM s (Typ s, Typ s, ReturnArity)
unifySelector = unifySelectorG absurdF

unifyAction :: UT.TaggedAction -> UnifyM s (Typ s, Typ s, ReturnArity)
unifyAction = unifySelectorG unifyExpr

unifySelectorG :: forall s expr. (expr D.Position -> UnifyM s (Typ s, Typ s, ReturnArity)) -> UT.Selector expr D.Position -> UnifyM s (Typ s, Typ s, ReturnArity)
unifySelectorG goExpr = \case
  UT.Compose _pos (s NE.:| rest) -> do
    s' <- unifySelectorG goExpr s
    foldlM compose s' rest
  UT.SplitFields pos _delim -> pure $ (T.textType pos, T.textType pos, Any)
  UT.SplitLines pos -> do
    pure (T.textType pos, T.textType pos, Any)
  UT.SplitWords pos -> do
    pure (T.textType pos, T.textType pos, Any)
  UT.Regex pos _regex bindings -> do
    declareBindings bindings
    pure $ (T.textType pos, T.textType pos, Any)
  UT.RegexMatches pos -> pure $ (T.regexMatchType pos, T.textType pos, Any)
  UT.RegexGroups pos _pat bindings -> do
    declareBindings bindings
    pure (T.textType pos, T.textType pos, Any)
  UT.ListOf pos inner -> do
    (inp, out, _arity) <- unifySelectorG goExpr inner
    pure (inp, T.listType pos out, Exactly 1)
  UT.Filter _pos _inner -> do
    inp <- freshVar
    out <- freshVar
    pure (inp, out, Affine)
  UT.Not _pos _inner -> do
    inp <- freshVar
    out <- freshVar
    pure (inp, out, Affine)
  UT.Splat pos -> do
    inp <- freshVar
    pure $ (T.listType pos inp, inp, Any)
  UT.Shell pos script shellMode -> do
    inp <- case shellMode of
      Normal -> pure $ T.textType pos
      NullStdin -> freshVar
    unifyBindingString inp script
    pure $ (inp, T.textType pos, Affine)
  UT.At pos _n -> do
    inp <- freshVar
    pure (T.listType pos inp, inp, Affine)
  UT.Take _pos _n inner -> do
    (inp, out, _arity) <- unifySelectorG goExpr inner
    pure (inp, out, Any)
  UT.TakeEnd _pos _n inner -> do
    (inp, out, _arity) <- unifySelectorG goExpr inner
    pure (inp, out, Any)
  UT.Drop _pos _n inner -> do
    (inp, out, _arity) <- unifySelectorG goExpr inner
    pure (inp, out, Any)
  UT.DropEnd _pos _n inner -> do
    (inp, out, _arity) <- unifySelectorG goExpr inner
    pure (inp, out, Any)
  UT.Contains pos _txt -> do
    pure $ (T.textType pos, T.textType pos, Affine)
  UT.Action _pos expr -> do
    goExpr expr
  UT.ParseJSON pos -> do
    pure $ (T.textType pos, T.jsonType pos, Exactly 1)
  UT.BindingAssignment _pos name -> do
    v <- initBinding name
    pure $ (v, v, Exactly 1)
  UT.Cast pos -> do
    inp <- freshVar
    pure $ (inp, T.castableType pos inp, Exactly 1)
  where
    compose ::
      (Typ s, Typ s, ReturnArity) ->
      UT.Selector expr Diagnose.Position ->
      UnifyM s (Typ s, Typ s, ReturnArity)
    compose (li, lm, lArity) rTagged = do
      (rm, ro, rArity) <- unifySelectorG goExpr rTagged
      _ <- liftUnify $ Unify.unify lm rm
      pure (li, ro, composeArity lArity rArity)

composeArity :: ReturnArity -> ReturnArity -> ReturnArity
composeArity = \cases
  Any _ -> Any
  _ Any -> Any
  Affine Affine -> Affine
  Affine Exactly {} -> Any
  Exactly {} Affine -> Any
  (Exactly n) (Exactly m) -> Exactly (n * m)

unifyExpr :: UT.TaggedExpr -> UnifyME (TypecheckFailure s) s (Typ s, Typ s, ReturnArity)
unifyExpr = \case
  Binding _pos InputBinding -> do
    inputTyp <- freshVar
    pure (inputTyp, inputTyp, Exactly 1)
  Binding pos (BindingName name) -> do
    bindingTyp <- expectBinding pos name
    -- any input type, output type matches the binding
    inputTyp <- freshVar
    pure (inputTyp, bindingTyp, Exactly 1)
  Str pos bindingStr -> do
    inputTyp <- freshVar
    unifyBindingString inputTyp bindingStr
    pure $ (inputTyp, (T.textType pos), Exactly 1)
  Number pos _num -> do
    inputTyp <- freshVar
    pure $ (inputTyp, (T.numberType pos), Exactly 1)
  StrConcat pos innerExpr -> do
    (inp, innerResult, _innerArity) <- unifyAction innerExpr
    _ <- liftUnify $ Unify.unify innerResult (T.listType pos (T.textType pos))
    -- TODO: Should We require the inner to have exactly 1 return?
    pure $ (inp, T.textType pos, Exactly 1)
  Intersperse _pos actions -> do
    typs <- for actions unifyAction <&> fmap \(i, o, _) -> (i, o)
    (i, o) <- liftUnify $ F1.foldrM1 (\(i1, o1) (i2, o2) -> (,) <$> Unify.unify i1 i2 <*> Unify.unify o1 o2) typs
    pure $ (i, o, Any)
  Comma _pos a b -> do
    (inp, out, _arity) <- unifyAction a
    (inp', out', _arity') <- unifyAction b
    _ <- liftUnify $ Unify.unify inp inp'
    _ <- liftUnify $ Unify.unify out out'
    pure $ (inp, out', Any)
  Count pos inner -> do
    (inp, _out, _arity) <- unifyAction inner
    pure $ (inp, T.numberType pos, Exactly 1)
  Plus pos a b -> do
    (inp, out, arity) <- unifyAction a
    (inp', out', arity') <- unifyAction b
    _ <- liftUnify $ Unify.unify out (T.numberType pos)
    o <- liftUnify $ Unify.unify out' (T.numberType pos)
    i <- liftUnify $ Unify.unify inp inp'
    pure $ (i, o, composeArity arity arity')

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
