{-# LANGUAGE EmptyCase #-}

module Focus.Typechecker
  ( typecheckModify,
  )
where

import Control.Lens
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT, withExceptT)
import Control.Monad.Logic (Logic)
import Control.Monad.Logic qualified as Logic
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.State (MonadState (..), StateT, evalStateT, mapStateT, modify)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Writer.CPS (WriterT, runWriterT)
import Control.Unification qualified as Unify
import Control.Unification.IntVar qualified as Unify
import Control.Unification.Types (UTerm (..))
import Data.Foldable1 qualified as F1
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Monoid (First (..))
import Data.Set.NonEmpty (NESet)
import Data.Text (Text)
import Data.Text qualified as Text
import Error.Diagnose qualified as D
import Error.Diagnose qualified as Diagnose
import Focus.Debug qualified as Debug
import Focus.Prelude
import Focus.Tagged (Tagged (..))
import Focus.Typechecker.Types (renderType)
import Focus.Typechecker.Types qualified as T
import Focus.Types
import Focus.Untyped (BindingName (..), Expr (..), Pos, TemplateString (..))
import Focus.Untyped qualified as UT

unificationErrorReport :: TypecheckFailure -> D.Report Text
unificationErrorReport = \case
  ExprInSelector pos ->
    Diagnose.Err
      Nothing
      "Expression found in selector"
      [(pos, D.This "This expression can't be used in the selector on the left-hand side of a '|='")]
      ["Certain expressions can't be sensibly 'reversed' as required by the use of '|='"]
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
  MismatchedInput pos typ ->
    Diagnose.Err
      Nothing
      "Type error"
      (expandPositions D.This ("The first selector must accept text input, but currently expects: " <> renderTyp typ) pos)
      []
  where
    varNames :: [Text]
    varNames =
      let letters = (Text.singleton <$> ['a' .. 'z'])
       in letters <> zipWith (<>) (cycle letters) (Text.pack . show <$> [(0 :: Int) ..])
    renderTyp :: Typ -> Text
    renderTyp = \case
      UVar v ->
        varNames !! succ (maxBound + Unify.getVarID v)
      UTerm t -> renderUTyp t
    renderUTyp :: ChunkTypeT (NESet D.Position) (Typ) -> Text
    renderUTyp = \case
      T.CastableTypeT _ typ -> "!" <> renderTyp typ
      T.Arrow _ a b -> "(" <> renderTyp a <> " -> " <> renderTyp b <> ")"
      T.TextTypeT {} -> renderType T.TextType
      T.ListTypeT _ t -> "[" <> renderTyp t <> "]"
      T.NumberTypeT _ -> renderType T.NumberType
      T.RegexMatchTypeT _ -> renderType T.RegexMatchType
      T.JsonTypeT _ -> renderType T.JsonType
      T.RecordTypeT _ fields -> "{" <> Text.intercalate ", " (M.toList fields <&> \(k, v) -> k <> ": " <> renderTyp v) <> "}"
      T.NullTypeT _ -> "null"

warningReport :: Warning -> D.Report Text
warningReport = \case {}

expandPositions :: (Foldable f) => (Text -> Diagnose.Marker Text) -> Text -> f D.Position -> [(D.Position, D.Marker Text)]
expandPositions marker msg xs = toList xs <&> \pos -> (pos, marker msg)

type UBindings = M.Map Text Typ

data Warning

type Warnings = [Warning]

type UnifyM = UnifyME TypecheckFailure

data UnifyEnv = UnifyEnv {inPathSelector :: Bool}
  deriving stock (Show, Eq, Ord)

type UMonad = Unify.IntBindingT (ChunkTypeT (NESet Pos)) Logic

type UnifyME e = (ReaderT UnifyEnv (WriterT Warnings (StateT (UBindings) (ExceptT (First e) UMonad))))

data TypecheckFailure
  = OccursFailure UVar Typ
  | MismatchFailure (ChunkTypeT (NESet Diagnose.Position) Typ) (ChunkTypeT (NESet Diagnose.Position) Typ)
  | UndeclaredBinding Diagnose.Position Text
  | ExpectedSingularArity Diagnose.Position ReturnArity
  | MismatchedInput (NESet Diagnose.Position) Typ
  | ExprInSelector Diagnose.Position

instance Unify.Fallible (ChunkTypeT (NESet D.Position)) UVar (First TypecheckFailure) where
  occursFailure a b = First . Just $ OccursFailure a b
  mismatchFailure a b = First . Just $ MismatchFailure a b

liftUnify :: (ExceptT (First TypecheckFailure) UMonad) a -> UnifyM a
liftUnify = lift . lift . lift

unifyBindings :: UBindings -> UnifyM ()
unifyBindings bindings =
  modify (bindings <>)

declareBindings :: BindingDeclarations -> UnifyM ()
declareBindings bd = do
  newBindings <- ifor bd $ \name (pos, typ) -> do
    v <- initBinding name
    liftUnify $ Unify.unify v (chunkTypeToChunkTypeT pos typ)
  unifyBindings newBindings
  where
    chunkTypeToChunkTypeT :: D.Position -> ChunkType -> Typ
    chunkTypeToChunkTypeT pos = \case
      T.TextType -> T.textType pos
      T.ListType t -> T.listType pos (chunkTypeToChunkTypeT pos t)
      T.NumberType -> T.numberType pos
      T.RegexMatchType -> T.regexMatchType pos
      T.JsonType -> T.jsonType pos
      T.RecordType fields -> T.recordType pos (chunkTypeToChunkTypeT pos <$> fields)

initBinding :: Text -> UnifyM (Typ)
initBinding name = do
  bindings <- get
  typ <- freshVar
  put $ M.insert name typ bindings
  pure typ

_getBinding :: Text -> D.Position -> UnifyM (Typ)
_getBinding name pos = do
  bindings <- get
  case M.lookup name bindings of
    Just v -> pure v
    Nothing -> throwError $ First . Just $ UndeclaredBinding pos name

expectBinding :: Diagnose.Position -> Text -> UnifyM (Typ)
expectBinding pos name = do
  bindings <- get
  case M.lookup name bindings of
    Just v -> pure v
    Nothing -> throwError . First . Just $ UndeclaredBinding pos name

typecheckModify :: (M.Map Text Typ) -> Typ -> UT.TaggedSelector -> Either TypeErrorReport [WarningReport]
typecheckModify initialBindings actualInp selector = do
  Debug.debugM "Init vars" initialBindings
  typecheckThing initialBindings False $ do
    (inp, _out, _arity) <- unifySelector selector
    expectInput actualInp inp
    pure ()

expectInput :: Typ -> Typ -> UnifyM ()
expectInput actualInp expectedInp =
  void . liftUnify $ withExceptT rewriteErr $ Unify.unify expectedInp actualInp
  where
    rewriteErr = \case
      First (Just (MismatchFailure a _)) -> First . Just $ MismatchedInput (tag a) (UTerm a)
      x -> x

typecheckThing :: (M.Map Text Typ) -> Bool -> (UnifyME TypecheckFailure ()) -> Either TypeErrorReport [WarningReport]
typecheckThing initialBindings warnOnExpr m = do
  let r = listToMaybe . Logic.observeMany 2 . Unify.runIntBindingT $ runExceptT $ flip evalStateT initialBindings $ mapStateT (withExceptT $ maybe mempty unificationErrorReport . getFirst) . runWriterT . flip runReaderT (UnifyEnv warnOnExpr) $ m
  case r of
    Just (r', _) -> do
      ((), warnings) <- r'
      pure $ warningReport <$> warnings
    Nothing -> error "Failed typechecking with no alternatives. Please report this error."

unifySelector :: UT.TaggedSelector -> UnifyM (Typ, Typ, ReturnArity)
unifySelector = unifySelectorG

unifyAction :: UT.TaggedSelector -> UnifyM (Typ, Typ, ReturnArity)
unifyAction = unifySelectorG

unifySelectorG :: UT.Selector D.Position -> UnifyM (Typ, Typ, ReturnArity)
unifySelectorG = \case
  UT.Id _pos -> do
    inp <- freshVar
    pure $ (inp, inp, Exactly 1)
  UT.Compose _pos (s NE.:| rest) -> do
    s' <- unifySelectorG s
    foldlM compose s' rest
  UT.SplitFields pos _delim -> pure $ (T.textType pos, T.textType pos, Any)
  UT.SplitLines pos -> do
    pure (T.textType pos, T.textType pos, Any)
  UT.Chars pos -> do
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
  UT.Reversed _pos inner -> do
    (inp, out, arity) <- unifySelectorG inner
    pure (inp, out, arity)
  UT.Contains pos _txt -> do
    pure $ (T.textType pos, T.textType pos, Affine)
  UT.Action _pos expr -> do
    unifyExpr expr
  UT.ParseJSON pos -> do
    pure $ (T.textType pos, T.jsonType pos, Exactly 1)
  UT.Cast pos -> do
    inp <- freshVar
    pure $ (inp, T.castableType pos inp, Exactly 1)
  UT.Noop _pos -> do
    inp <- freshVar
    out <- freshVar
    pure $ (inp, out, Exactly 0)
  UT.Prompt pos -> do
    inp <- freshVar
    pure $ (inp, T.textType pos, Exactly 1)
  UT.File pos fileSelector -> do
    (inp, fpOut, _arity) <- asView $ unifySelectorG fileSelector
    _ <- liftUnify $ Unify.unify fpOut (T.textType pos)
    out <- freshVar
    pure $ (inp, out, Any)
  UT.DebugTrace _pos labelSelector -> do
    (inp, _labelOut, _arity) <- asView $ unifySelectorG labelSelector
    pure $ (inp, inp, Any)
  where
    compose ::
      (Typ, Typ, ReturnArity) ->
      UT.Selector Diagnose.Position ->
      UnifyM (Typ, Typ, ReturnArity)
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

asView :: (MonadReader UnifyEnv m) => m r -> m r
asView = local \e -> e {inPathSelector = False}

unifyExpr :: UT.TaggedExpr -> UnifyME (TypecheckFailure) (Typ, Typ, ReturnArity)
unifyExpr expr = do
  exprWarning
  case expr of
    UT.Modify _pos selector modifier -> do
      (selInp, selOut, selArity) <- local (\e -> e {inPathSelector = True}) $ unifySelectorG selector
      (modInp, modOut, _modArity) <- unifySelectorG modifier
      _ <- liftUnify $ Unify.unify selOut modInp
      _ <- liftUnify $ Unify.unify selOut modOut
      pure (selInp, selInp, selArity)
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
    StrAppend pos innerL innerR -> do
      (lInp, lOut, _innerArity) <- unifyAction innerL
      (rInp, rOut, _innerArity) <- unifyAction innerR
      inp <- liftUnify $ Unify.unify lInp rInp
      out <- liftUnify $ Unify.unify lOut rOut
      out' <- liftUnify $ Unify.unify out (T.textType pos)
      pure $ (inp, out', Exactly 1)
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
      unifyBindings (fields' <&> \(_i, o, _arity) -> o)
      pure $ (inp', T.recordType pos (view _2 <$> fields'), arity)
    UT.Cycle _pos inner -> do
      (inp, out, innerArity) <- unifyAction inner
      let arity = case innerArity of
            Exactly 0 -> Exactly 0
            Affine -> Any
            _ -> Infinite
      pure $ (inp, out, arity)
    UT.BindingAssignment _pos inner name -> do
      v <- initBinding name
      (i, o, arity) <- unifySelector inner
      _ <- liftUnify $ Unify.unify v o
      pure $ (i, i, arity)
    UT.Index pos -> do
      pure $ (T.textType pos, T.numberType pos, Exactly 1)
    UT.Uniq _pos inner -> do
      (inp, out, _arity) <- unifySelector inner
      pure $ (inp, out, Any)
  where
    exprWarning :: UnifyM ()
    exprWarning = do
      asks inPathSelector >>= \case
        True -> do
          throwError . First . Just $ ExprInSelector (tag expr)
        False -> do
          pure ()

unifyTemplateString :: Typ -> Diagnose.Position -> TemplateString D.Position -> UnifyM (Typ, Typ, ReturnArity)
unifyTemplateString inputTyp pos (TemplateString bindings) = do
  arities <- for bindings $ \case
    Left sel -> do
      (i, o, arity) <- unifySelectorG sel
      _ <- liftUnify $ Unify.unify i inputTyp
      _ <- liftUnify $ Unify.unify o (T.textType (tag sel))
      pure arity
    Right _ -> pure (Exactly 1)
  pure (inputTyp, T.textType pos, composeArities (Exactly 1 NE.:| arities))

freshVar :: UnifyM (Typ)
freshVar = (Unify.UVar <$> (lift . lift . lift . lift $ Unify.freeVar))
