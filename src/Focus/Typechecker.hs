{-# LANGUAGE EmptyCase #-}

module Focus.Typechecker
  ( typecheckModify,
  )
where

import Control.Lens
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT, withExceptT)
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
import Focus.Untyped (Expr (..), Pos, TemplateString (..))
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
  UndeclaredBinding pos (UT.BindingName name) ->
    Diagnose.Err
      Nothing
      "Unknown binding"
      [(pos, D.This $ "The binding " <> name <> " is not in scope.")]
      []
  ExpectedSingularArity pos arity ->
    Diagnose.Err
      Nothing
      "Multiplicity error"
      [(pos, D.This $ "Actions must result in exactly one result." <> tShow arity)]
      []
  MismatchedInput pos typ actual ->
    Diagnose.Err
      Nothing
      "Type error"
      (expandPositions D.This ("This usage expects the first input to be of type " <> renderTyp typ <> ", but it's actually: " <> renderTyp actual) pos)
      [ Diagnose.Hint "Did you forget to provide a file or '-' for stdin as input?"
      ]
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
      T.RecordTypeT _ fields -> "{" <> Text.intercalate ", " (M.toList fields <&> \(UT.BindingName k, v) -> k <> ": " <> renderTyp v) <> "}"
      T.NullTypeT _ -> renderType T.NullType
      T.TupleTypeT _ ts -> "(" <> Text.intercalate ", " (renderTyp <$> ts) <> ")"

warningReport :: Warning -> D.Report Text
warningReport = \case {}

expandPositions :: (Foldable f) => (Text -> Diagnose.Marker Text) -> Text -> f D.Position -> [(D.Position, D.Marker Text)]
expandPositions marker msg xs = toList xs <&> \pos -> (pos, marker msg)

type UBindings = M.Map UT.BindingName Typ

data Warning

type Warnings = [Warning]

type UnifyM = UnifyME TypecheckFailure

data UnifyEnv = UnifyEnv {inPathSelector :: Bool}
  deriving stock (Show, Eq, Ord)

type UMonad = Unify.IntBindingT (ChunkTypeT (NESet Pos)) Identity

type UnifyME e = (ReaderT UnifyEnv (WriterT Warnings (StateT UBindings (ExceptT e UMonad))))

data TypecheckFailure
  = OccursFailure UVar Typ
  | MismatchFailure (ChunkTypeT (NESet Diagnose.Position) Typ) (ChunkTypeT (NESet Diagnose.Position) Typ)
  | UndeclaredBinding Diagnose.Position UT.BindingName
  | ExpectedSingularArity Diagnose.Position ReturnArity
  | MismatchedInput (NESet Diagnose.Position) Typ Typ
  | ExprInSelector Diagnose.Position

instance Unify.Fallible (ChunkTypeT (NESet D.Position)) UVar TypecheckFailure where
  occursFailure a b = OccursFailure a b
  mismatchFailure a b = MismatchFailure a b

liftUnify :: (ExceptT (TypecheckFailure) UMonad) a -> UnifyM a
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
      T.NullType -> T.nullType pos
      T.TupleType ts -> T.tupleType pos (chunkTypeToChunkTypeT pos <$> ts)

initBinding :: UT.BindingName -> UnifyM Typ
initBinding name = do
  bindings <- get
  typ <- freshVar
  put $ M.insert name typ bindings
  pure typ

expectBinding :: Diagnose.Position -> UT.BindingName -> UnifyM (Typ)
expectBinding pos name = do
  bindings <- get
  case M.lookup name bindings of
    Just v -> pure v
    Nothing -> throwError $ UndeclaredBinding pos name

typecheckModify :: (M.Map UT.BindingName Typ) -> Typ -> UT.TaggedSelector -> Either TypeErrorReport [WarningReport]
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
      MismatchFailure a b -> MismatchedInput (tag a) (UTerm a) (UTerm b)
      x -> x

typecheckThing :: (M.Map UT.BindingName Typ) -> Bool -> (UnifyME TypecheckFailure ()) -> Either TypeErrorReport [WarningReport]
typecheckThing initialBindings warnOnExpr m = do
  let r = Unify.runIntBindingT $ runExceptT $ flip evalStateT initialBindings $ mapStateT (withExceptT unificationErrorReport) . runWriterT . flip runReaderT (UnifyEnv warnOnExpr) $ m
  case r of
    Identity (r', _) -> do
      ((), warnings) <- r'
      pure $ warningReport <$> warnings

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
  UT.Empty _pos -> do
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

zipArities :: (Foldable f) => f ReturnArity -> ReturnArity
zipArities = foldl' matchArities Infinite

matchArities :: ReturnArity -> ReturnArity -> ReturnArity
matchArities = \cases
  x Infinite -> x
  Infinite x -> x
  Any _ -> Any
  _ Any -> Any
  Affine Affine -> Affine
  Affine Exactly {} -> Affine
  Exactly {} Affine -> Affine
  (Exactly n) (Exactly m) -> Exactly (min n m)

unifyAll :: (Foldable f) => f Typ -> UnifyM Typ
unifyAll xs = do
  case toList xs of
    [] -> freshVar
    [x] -> pure x
    (x : xs') -> do
      foldM (\a b -> liftUnify $ Unify.unify a b) x xs'

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
    Binding pos name -> do
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
      let (inputs, outputs, arities) = (fields' <&> view _1, fields' <&> view _2, fields' <&> view _3)
      inp <- unifyAll inputs
      let arity = zipArities arities
      unifyBindings outputs
      pure $ (inp, T.recordType pos outputs, arity)
    UT.Cycle _pos inner -> do
      (inp, out, innerArity) <- unifyAction inner
      let arity = case innerArity of
            Exactly 0 -> Exactly 0
            Affine -> Any
            _ -> Infinite
      pure $ (inp, out, arity)
    UT.Pattern _pos pat -> do
      unifyPattern pat
    UT.Index pos -> do
      pure $ (T.textType pos, T.numberType pos, Exactly 1)
    UT.Uniq _pos inner -> do
      (inp, out, _arity) <- unifySelector inner
      pure $ (inp, out, Any)
    UT.Select _pos branches -> do
      -- Each branch cond/body can have different types as long as the cond input and body
      -- output match with all other branches.
      branchTypes <- for branches \(cond, body) -> do
        (condI, condO, condArity) <- unifySelectorG cond
        (bodyI, bodyO, bodyArity) <- unifySelectorG body
        _ <- liftUnify $ Unify.unify condO bodyI
        pure (condI, bodyO, composeArities (condArity NE.:| [bodyArity]))
      zipTypeSigs (toList branchTypes)
    UT.Zip pos fields -> do
      fieldsTypes <- for fields unifySelectorG
      let (inputs, outputs, arities) = (fieldsTypes <&> view _1, fieldsTypes <&> view _2, fieldsTypes <&> view _3)
      inp <- unifyAll inputs
      let arity = zipArities arities
      pure $ (inp, T.tupleType pos outputs, arity)
    UT.Chain _pos fields -> do
      fieldsTypes <- for fields unifySelectorG
      zipTypeSigs fieldsTypes
  where
    exprWarning :: UnifyM ()
    exprWarning = do
      asks inPathSelector >>= \case
        True -> do
          throwError $ ExprInSelector (tag expr)
        False -> do
          pure ()

unifyPattern :: UT.Pattern D.Position -> UnifyM (Typ, Typ, ReturnArity)
unifyPattern = \case
  UT.BindingPattern _pos name -> do
    v <- initBinding name
    pure $ (v, v, Exactly 1)
  UT.PatternString pos bindings _re -> do
    declareBindings bindings
    pure $ (T.textType pos, T.textType pos, Affine)
  UT.PatternList _pos pats -> do
    for pats unifyPattern >>= zipTypeSigs

-- | Unify many (input, output, arity) asserting that all the inputs unify, all the outputs unify, and the arities are combined
zipTypeSigs :: (Foldable f) => f (Typ, Typ, ReturnArity) -> UnifyM (Typ, Typ, ReturnArity)
zipTypeSigs xs = do
  let (is, os, as) = unzip3 (toList xs)
  i <- unifyAll is
  o <- unifyAll os
  let a = zipArities as
  pure (i, o, a)

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
