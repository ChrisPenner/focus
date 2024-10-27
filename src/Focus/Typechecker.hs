module Focus.Typechecker
  ( typecheckSelector,
    typecheckModify,
    typecheckView,
  )
where

import Control.Lens
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT, withExceptT)
import Control.Monad.Reader (ReaderT (..), asks)
import Control.Monad.State (MonadState (..), StateT, evalStateT, mapStateT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Writer.CPS (WriterT, runWriterT)
import Control.Monad.Writer (MonadWriter (..))
import Control.Unification qualified as Unify
import Control.Unification.STVar qualified as Unify
import Control.Unification.Types (UTerm (..))
import Data.Foldable1 qualified as F1
import Data.List.NonEmpty (NonEmpty)
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
import Focus.Untyped (BindingName (..), Expr (..), TemplateString (..))
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
      T.RecordTypeT _ fields -> "{" <> Text.intercalate ", " (M.toList fields <&> \(k, v) -> k <> ": " <> renderTyp v) <> "}"

warningReport :: Warning -> D.Report Text
warningReport = \case
  ExprInSelector pos ->
    Diagnose.Warn
      Nothing
      "Expression found in selector"
      [(pos, D.This "This expression isn't reversable.")]
      [ D.Note "You can still run the selector, but any results will be passed through it unaltered."
      ]

expandPositions :: (Foldable f) => (Text -> Diagnose.Marker Text) -> Text -> f D.Position -> [(D.Position, D.Marker Text)]
expandPositions marker msg xs = toList xs <&> \pos -> (pos, marker msg)

type UBindings s = M.Map Text (Typ s)

data Warning
  = ExprInSelector Diagnose.Position
  deriving stock (Show, Eq, Ord)

type Warnings = [Warning]

type UnifyM s = UnifyME (TypecheckFailure s) s

data UnifyEnv = UnifyEnv {warnExpr :: Bool}
  deriving stock (Show, Eq, Ord)

type UnifyME e s = (ReaderT UnifyEnv (WriterT Warnings (StateT (UBindings s) (ExceptT e (Unify.STBinding s)))))

data TypecheckFailure s
  = OccursFailure (UVar s) (Typ s)
  | MismatchFailure (ChunkTypeT (NESet Diagnose.Position) (Typ s)) (ChunkTypeT (NESet Diagnose.Position) (Typ s))
  | UndeclaredBinding Diagnose.Position Text
  | ExpectedSingularArity Diagnose.Position ReturnArity
  | NonTextInput (NESet Diagnose.Position) (Typ s)

instance Unify.Fallible (ChunkTypeT (NESet D.Position)) (UVar s) (TypecheckFailure s) where
  occursFailure = OccursFailure
  mismatchFailure = MismatchFailure

liftUnify :: (ExceptT (TypecheckFailure s) (Unify.STBinding s)) a -> UnifyM s a
liftUnify = lift . lift . lift

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
      T.RecordType fields -> T.recordType pos (chunkTypeToChunkTypeT pos <$> fields)

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

typecheckModify :: UT.TaggedSelector -> Either TypeErrorReport [WarningReport]
typecheckModify selector = do
  typecheckThing False $ do
    (inp, _out, _arity) <- unifySelector selector
    expectTextInput inp (tag selector)
    pure ()

typecheckView :: UT.TaggedSelector -> Either TypeErrorReport [WarningReport]
typecheckView action = typecheckThing False $ do
  (inp, _out, _arity) <- unifyAction action
  expectTextInput inp (tag action)

expectTextInput :: Typ s -> Diagnose.Position -> UnifyM s ()
expectTextInput inp pos =
  void . liftUnify $ withExceptT rewriteErr $ Unify.unify inp (T.textType pos)
  where
    rewriteErr = \case
      MismatchFailure a _ -> NonTextInput (tag a) (UTerm a)
      x -> x

-- unifyModify :: (UT.TaggedSelector, UT.Selector D.Position) -> UnifyME (TypecheckFailure s) s (Typ s, Typ s, ReturnArity)
-- unifyModify (selector, expr) = do
--   (selectorIn, selectorOut, _selArity) <- unifySelector selector
--   (exprIn, exprOut, actionArity) <- unifyAction expr
--   _ <- liftUnify $ Unify.unify selectorOut exprIn
--   _ <- liftUnify $ Unify.unify selectorOut exprOut
--   -- case actionArity of
--   --   Exactly 1 -> pure ()
--   --   _ -> throwError $ ExpectedSingularArity (tag expr) actionArity
--   pure (selectorIn, exprOut, actionArity)

typecheckSelector :: Bool -> UT.TaggedSelector -> Either TypeErrorReport [WarningReport]
typecheckSelector warnOnExpr sel = typecheckThing warnOnExpr do
  (inp, _out, _arity) <- unifySelector sel
  expectTextInput inp (tag sel)
  pure ()

typecheckThing :: Bool -> (forall s. UnifyME (TypecheckFailure s) s ()) -> Either TypeErrorReport [WarningReport]
typecheckThing warnOnExpr m = do
  (_, warnings) <- Unify.runSTBinding $ runExceptT $ flip evalStateT mempty $ mapStateT (withExceptT unificationErrorReport) . runWriterT . flip runReaderT (UnifyEnv warnOnExpr) $ void $ m
  pure $ warningReport <$> warnings

unifySelector :: UT.TaggedSelector -> UnifyM s (Typ s, Typ s, ReturnArity)
unifySelector = unifySelectorG

unifyAction :: UT.TaggedSelector -> UnifyM s (Typ s, Typ s, ReturnArity)
unifyAction = unifySelectorG

unifySelectorG :: forall s. UT.Selector D.Position -> UnifyM s (Typ s, Typ s, ReturnArity)
unifySelectorG = \case
  UT.Compose _pos (s NE.:| rest) -> do
    s' <- unifySelectorG s
    foldlM compose s' rest
  UT.Modify _pos selector modifier -> do
    -- TODO: add warning if modifier found in another modifier
    (selInp, selOut, selArity) <- unifySelectorG selector
    (modInp, modOut, _modArity) <- unifySelectorG modifier
    _ <- liftUnify $ Unify.unify selOut modInp
    _ <- liftUnify $ Unify.unify selOut modOut
    pure (selInp, selInp, selArity)
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
    (inp, out, _arity) <- unifySelectorG inner
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
  UT.At pos _n -> do
    inp <- freshVar
    pure (T.listType pos inp, inp, Affine)
  UT.Take _pos _n inner -> do
    (inp, out, _arity) <- unifySelectorG inner
    pure (inp, out, Any)
  UT.TakeEnd _pos _n inner -> do
    (inp, out, _arity) <- unifySelectorG inner
    pure (inp, out, Any)
  UT.Drop _pos _n inner -> do
    (inp, out, _arity) <- unifySelectorG inner
    pure (inp, out, Any)
  UT.DropEnd _pos _n inner -> do
    (inp, out, _arity) <- unifySelectorG inner
    pure (inp, out, Any)
  UT.Contains pos _txt -> do
    pure $ (T.textType pos, T.textType pos, Affine)
  UT.Action _pos expr -> do
    unifyExpr expr
  UT.ParseJSON pos -> do
    pure $ (T.textType pos, T.jsonType pos, Exactly 1)
  UT.BindingAssignment _pos name -> do
    v <- initBinding name
    pure $ (v, v, Exactly 1)
  UT.Cast pos -> do
    inp <- freshVar
    pure $ (inp, T.castableType pos inp, Exactly 1)
  UT.Record pos fields -> do
    fields' <- for fields unifySelectorG
    inp <- freshVar
    inp' <-
      foldM
        ( \i1 i2 -> do
            liftUnify $ Unify.unify i1 i2
        )
        inp
        (view _1 <$> fields')
    let arity = foldl zipArities Infinite (view _3 <$> fields')
    pure $ (inp', T.recordType pos (view _2 <$> fields'), arity)
  where
    compose ::
      (Typ s, Typ s, ReturnArity) ->
      UT.Selector Diagnose.Position ->
      UnifyM s (Typ s, Typ s, ReturnArity)
    compose (li, lm, lArity) rTagged = do
      (rm, ro, rArity) <- unifySelectorG rTagged
      _ <- liftUnify $ Unify.unify lm rm
      pure (li, ro, composeArity lArity rArity)

composeArities :: NonEmpty ReturnArity -> ReturnArity
composeArities xs = F1.foldl1' composeArity xs

composeArity :: ReturnArity -> ReturnArity -> ReturnArity
composeArity = \cases
  Any _ -> Any
  _ Any -> Any
  Affine Affine -> Affine
  Affine Exactly {} -> Any
  Affine Infinite -> Any
  (Exactly 0) Infinite -> Exactly 0
  Infinite (Exactly 0) -> Exactly 0
  Infinite (Exactly _) -> Infinite
  (Exactly _) Infinite -> Infinite
  Infinite Affine -> Any
  Exactly {} Affine -> Any
  Infinite Infinite -> Infinite
  (Exactly n) (Exactly m) -> Exactly (n * m)

zipArities :: ReturnArity -> ReturnArity -> ReturnArity
zipArities = \cases
  x Infinite -> x
  Infinite x -> x
  Any _ -> Any
  _ Any -> Any
  Affine Affine -> Affine
  Affine Exactly {} -> Affine
  Exactly {} Affine -> Affine
  (Exactly n) (Exactly m) -> Exactly (min n m)

unifyExpr :: UT.TaggedExpr -> UnifyME (TypecheckFailure s) s (Typ s, Typ s, ReturnArity)
unifyExpr expr = do
  exprWarning
  case expr of
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
      unifyTemplateString inputTyp pos bindingStr
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
    Shell pos script shellMode -> do
      inp <- case shellMode of
        Normal -> pure $ T.textType pos
        NullStdin -> freshVar
      (i, o, arity) <- unifyTemplateString inp pos script
      pure (i, o, composeArity arity (Exactly 1))
    MathBinOp pos _op a b -> do
      (inp, out, arity) <- unifyAction a
      (inp', out', arity') <- unifyAction b
      _ <- liftUnify $ Unify.unify out (T.numberType pos)
      o <- liftUnify $ Unify.unify out' (T.numberType pos)
      i <- liftUnify $ Unify.unify inp inp'
      pure $ (i, o, composeArity arity arity')
  where
    exprWarning :: UnifyM s ()
    exprWarning = do
      asks warnExpr >>= \case
        True -> do
          tell $ [ExprInSelector (tag expr)]
        False -> do
          pure ()

unifyTemplateString :: Typ s -> Diagnose.Position -> TemplateString D.Position -> UnifyM s (Typ s, Typ s, ReturnArity)
unifyTemplateString inputTyp pos (TemplateString bindings) = do
  arities <- for bindings $ \case
    Left sel -> do
      (i, o, arity) <- unifySelectorG sel
      _ <- liftUnify $ Unify.unify i inputTyp
      _ <- liftUnify $ Unify.unify o (T.textType (tag sel))
      pure arity
    Right _ -> pure (Exactly 1)
  pure (inputTyp, T.textType pos, composeArities (Exactly 1 NE.:| arities))

freshVar :: UnifyM s (Typ s)
freshVar = (Unify.UVar <$> (lift . lift . lift . lift $ Unify.freeVar))
