{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Focus.Compile
  ( compileSelector,
    Focus (..),
    FocusM (..),
    textChunk,
    listChunk,
    SelectorError (..),
  )
where

import Control.Lens hiding (Empty, Reversed)
import Control.Lens.Regex.Text qualified as RE
import Control.Monad.Coroutine (Coroutine)
import Control.Monad.Coroutine qualified as Co
import Control.Monad.Coroutine.SuspensionFunctors qualified as Co
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Writer.CPS qualified as Writer
import Data.Aeson qualified as Aeson
import Data.Align
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Monoid (First (First, getFirst))
import Data.Monoid qualified as Monoid
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.These
import Error.Diagnose qualified as D
import Focus.Command (CommandF (..), CommandT (..), IsCmd)
import Focus.Debug qualified as Debug
import Focus.Focus
import Focus.IO qualified as IO
import Focus.Prelude
import Focus.Tagged (Tagged (..))
import Focus.Types
import Focus.Untyped
import GHC.Stack (HasCallStack)
import System.Exit (ExitCode (..))
import UnliftIO qualified
import UnliftIO.Directory qualified as UnliftIO
import UnliftIO.Process qualified as UnliftIO
import UnliftIO.STM
import Prelude hiding (reads)

compileSelector :: forall cmd. (IsCmd cmd) => CommandF cmd -> TaggedSelector -> IO (Focus cmd Chunk Chunk)
compileSelector cmdF = compileSelectorG cmdF

data Pair a = Pair {pairL :: a, pairR :: a}
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

compileExpr :: TaggedExpr -> IO (Focus ViewT Chunk Chunk)
compileExpr =
  \case
    Modify _pos sel modifier -> do
      selF <- compileSelectorG ModifyF sel
      modF <- compileSelectorG ViewF modifier
      pure $ ViewFocus $ \f inp -> do
        r <-
          inp & getModifyFocus selF \theFocus -> do
            viewFirst modF theFocus
        f r
    Binding pos bindingName -> do
      pure $ ViewFocus \f inp -> do
        binding <- resolveBinding pos inp bindingName
        f binding
    Str _pos (TemplateString xs) -> do
      compiledFocs <- xs & traversed . _Left %%~ compileSelectorG ViewF
      let handler :: forall m r. (Focusable m, Monoid r) => (Chunk -> m r) -> Chunk -> m r
          handler f inp = do
            compileTemplateString inp compiledFocs \txt -> do
              f txt
      pure $ ViewFocus handler
    Number _pos num -> pure $ liftTrav $ \f _ -> f (NumberChunk num)
    StrConcat _pos innerExpr -> do
      inner <- compileSelector ViewF innerExpr
      let go :: ((Chunk -> m c) -> a -> m b) -> (Chunk -> m c) -> a -> m b
          go vf f inp =
            inp & vf \xs -> do
              f . TextChunk $ Text.concat (textChunk <$> listChunk xs)
      pure $ ViewFocus $ \f inp -> go (getViewFocus inner) f inp
    StrAppend _pos innerL innerR -> do
      vf <- emitZipped (Pair innerL innerR)
      pure $ ViewFocus $ \f inp -> do
        let foc = getViewFocus vf
        inp & foc \(Pair l r) -> do
          f (TextChunk $ textChunk l <> textChunk r)
    Intersperse _pos actions -> do
      selectors <- traverse (compileSelector ViewF) actions
      let actionFocuses = (listOfFocus <$> selectors)
      pure $ ViewFocus $ \f inp -> do
        chunkResults <- for actionFocuses \af -> do
          inp & getViewFocus af pure
        let go = \case
              [] -> pure mempty
              ([] : rest) -> go rest
              ((x : xs) : rest) -> do
                r <- f x
                (r <>) <$> go (rest ++ [xs])
        go (NE.toList chunkResults)
    Count _ selector -> do
      inner <- compileSelectorG ViewF selector
      pure $ listOfFocus inner >.> focusTo (pure . NumberChunk . IntNumber . length)
    Shell pos (TemplateString xs) shellMode -> do
      compiledFocs <- xs & traversed . _Left %%~ compileSelectorG ViewF
      pure $ ViewFocus \f inp -> do
        compileTemplateString inp compiledFocs \txt -> do
          let shellTxt = textChunk txt
          let proc = UnliftIO.shell (Text.unpack shellTxt)
          let proc' = proc {UnliftIO.std_in = UnliftIO.CreatePipe, UnliftIO.std_out = UnliftIO.CreatePipe, UnliftIO.std_err = UnliftIO.CreatePipe}
          procResult <- liftIO $ UnliftIO.withCreateProcess proc' \mstdin mstdout mstderr phandle -> do
            let stdin = fromMaybe (error "missing stdin") mstdin
            let stdout = fromMaybe (error "missing stdout") mstdout
            let stderr = fromMaybe (error "missing stderr") mstderr
            case shellMode of
              Normal -> liftIO $ Text.hPutStrLn stdin (textChunk inp)
              NullStdin -> pure ()
            UnliftIO.hClose stdin
            UnliftIO.waitForProcess phandle >>= \case
              ExitFailure code -> do
                errOut <- liftIO $ Text.hGetContents stderr
                pure $ Left (code, errOut)
              ExitSuccess -> do
                out <- liftIO $ Text.hGetContents stdout
                let out' = fromMaybe out $ Text.stripSuffix "\n" out
                pure $ Right out'
          case procResult of
            Left (code, errOut) -> do
              mayErr $ ShellError pos $ "Shell script failed with exit code " <> tShow code <> ": " <> "\n" <> errOut
              pure mempty
            Right out -> do
              f (TextChunk out)
    MathBinOp pos operation ls rs -> do
      l <- compileSelectorG ViewF ls
      r <- compileSelectorG ViewF rs
      pure $ ViewFocus $ \f inp -> do
        inp & getViewFocus l \lchunk -> do
          inp & getViewFocus r \rchunk -> do
            case (castNumber lchunk, castNumber rchunk) of
              (Just ln, Just rn) -> do
                case runNumberOp operation ln rn of
                  Left err -> do
                    mayErr $ MathError pos err
                    pure mempty
                  Right nt -> f $ NumberChunk nt
              (Nothing, _) -> do
                mayErr $ CastError (tag ls) NumberType lchunk
                pure mempty
              (_, Nothing) -> do
                mayErr $ CastError (tag rs) NumberType rchunk
                pure mempty
      where
        runNumberOp :: MathBinOp -> NumberT -> NumberT -> Either Text NumberT
        runNumberOp = \cases
          Plus (IntNumber x) (IntNumber y) -> Right $ IntNumber (x + y)
          Plus x y -> Right $ DoubleNumber $ onDoubles (+) x y
          Minus (IntNumber x) (IntNumber y) -> Right $ IntNumber (x - y)
          Minus x y -> Right $ DoubleNumber $ onDoubles (-) x y
          Multiply (IntNumber x) (IntNumber y) -> Right $ IntNumber (x * y)
          Multiply x y -> Right $ DoubleNumber $ onDoubles (*) x y
          Divide _ (IntNumber 0) -> Left "Division by zero"
          Divide (IntNumber x) (IntNumber y) -> Right $ DoubleNumber (fromIntegral x / fromIntegral y)
          Divide x y -> Right $ DoubleNumber $ onDoubles (/) x y
          Modulo (IntNumber x) (IntNumber y) -> Right $ IntNumber (x `mod` y)
          Modulo _x _y -> Left "Can't use modulo on floating point numbers"
          Power (IntNumber x) (IntNumber y) -> Right $ IntNumber (x ^ y)
          Power x y -> Right $ DoubleNumber $ onDoubles (**) x y

        onDoubles :: (Double -> Double -> x) -> NumberT -> NumberT -> x
        onDoubles f = \cases
          (IntNumber i) (IntNumber j) -> f (fromIntegral i) (fromIntegral j)
          (IntNumber i) (DoubleNumber j) -> f (fromIntegral i) j
          (DoubleNumber i) (IntNumber j) -> f i (fromIntegral j)
          (DoubleNumber i) (DoubleNumber j) -> f i j
    Record _pos fields -> do
      compileRecord fields
    Cycle _pos selector -> do
      s <- compileSelectorG ViewF selector
      pure $
        listOfFocus s >.> liftTrav \f xs -> do
          for (cycle xs) f
    Pattern _pos pat -> do
      compilePatternG pat
    Index _pos -> do
      counter <- newTVarIO 0
      pure $ ViewFocus \f _inp -> do
        i <- atomically $ do
          i <- readTVar counter
          modifyTVar' counter (+ 1)
          pure i
        f (NumberChunk $ IntNumber i)
    Uniq _pos inner -> do
      innerT <- compileSelectorG ViewF inner
      let go :: forall r m. (Focusable m, Monoid r) => (Chunk -> m r) -> Chunk -> m r
          go f chunk =
            let foc :: (Chunk -> StateT (Set Chunk) m r) -> Chunk -> StateT (Set Chunk) m r
                foc = getViewFocus innerT
             in flip evalStateT mempty $ do
                  chunk
                    & foc
                      %%~ ( \focChunk -> do
                              seen <- get
                              modify (Set.insert focChunk)
                              if Set.member focChunk seen
                                then pure mempty
                                else lift $ f focChunk
                          )
      pure $ ViewFocus go
    Select _pos branches -> condBranches (toList branches)
    Zip _pos fields -> do
      fv <- emitZipped fields
      pure $ ViewFocus \f chunk -> do
        let foc = getViewFocus fv
        chunk & foc \rec -> do
          f (TupleChunk rec)
    Chain _pos fields -> do
      focs <- for fields (compileSelectorG ViewF)
      pure $ ViewFocus \f inp -> do
        focs & foldMapM \foc -> getViewFocus foc f inp
    Product _pos fields -> do
      rmap TupleChunk <$> emitProduct fields
  where
    compileTemplateString :: (Focusable m, Monoid r) => Chunk -> [Either (Focus ViewT Chunk Chunk) Text] -> (Chunk -> m r) -> m r
    compileTemplateString inp compiledFocs f = do
      compileTemplateStringHelper inp compiledFocs f
    compileTemplateStringHelper :: (Focusable m, Monoid r) => Chunk -> [Either (Focus ViewT Chunk Chunk) Text] -> (Chunk -> m r) -> m r
    compileTemplateStringHelper inp xs f =
      case xs of
        [] -> f (TextChunk "")
        (Left foc : rest) -> do
          case foc of
            ViewFocus vf -> do
              inp
                & vf
                  ( \t ->
                      compileTemplateStringHelper inp rest \restT ->
                        f (TextChunk $ textChunk t <> textChunk restT)
                  )
        Right txt : rest -> do
          compileTemplateStringHelper inp rest (\t -> f . TextChunk $ (txt <> textChunk t))

compileSelectorG :: forall cmd. (IsCmd cmd) => CommandF cmd -> Selector D.Position -> IO (Focus cmd Chunk Chunk)
compileSelectorG cmdF = \case
  Id _ -> pure $ focusId
  Compose _ xs -> do
    compiled <- traverse (compileSelectorG cmdF) xs
    pure $ foldr1 (>.>) compiled
  SplitFields _ delim -> pure $ underText $ liftTrav (\f txt -> (Text.intercalate delim) <$> traverse f (Text.splitOn delim txt))
  SplitLines _ -> pure $ underText $ liftTrav (\f txt -> Text.unlines <$> traverse f (Text.lines txt))
  Chars _pos -> pure $ underText $ liftTrav $ \f txt -> Text.concat <$> traverse f (Text.singleton <$> Text.unpack txt)
  SplitWords _ -> pure $ underText $ liftTrav $ (\f txt -> Text.unwords <$> traverse f (Text.words txt))
  Regex _ pat _ -> do
    case cmdF of
      ViewF -> do pure $ viewRegex pat RE.match
      ModifyF -> do pure $ modifyRegex pat RE.match
  RegexMatches _ ->
    pure $ liftTrav $ _RegexMatchChunk . RE.match . from textI
  RegexGroups _ pat _ -> do
    pure $ case cmdF of
      ViewF -> do viewRegex pat (RE.groups . traverse)
      ModifyF -> do modifyRegex pat (RE.groups . traverse)
  ListOf _ selector -> do
    compiled <- compileSelectorG cmdF selector
    pure $ listOfFocus compiled >.> liftTrav (from asListI)
  Filter _ selector -> do
    inner <- compileSelectorG cmdF selector
    pure $ case cmdF of
      ViewF -> do
        ViewFocus $ \f chunk -> do
          hasMatches cmdF inner chunk >>= \case
            True -> f chunk
            False -> pure mempty
      ModifyF -> ModifyFocus $ \f chunk -> do
        hasMatches cmdF inner chunk >>= \case
          True -> f chunk
          False -> pure chunk
  Not _ selector -> do
    inner <- compileSelectorG cmdF selector
    pure $ case cmdF of
      ViewF -> do
        ViewFocus $ \f chunk -> do
          hasMatches cmdF inner chunk >>= \case
            True -> pure mempty
            False -> f chunk
      ModifyF -> ModifyFocus $ \f chunk -> do
        hasMatches cmdF inner chunk >>= \case
          True -> pure chunk
          False -> f chunk
  Splat _ -> pure $ do
    liftTrav $ \f chunk -> do
      listChunk chunk
        & traversed %%~ f
        <&> ListChunk
  At _ n -> pure $ do
    liftTrav $ \f chunk -> do
      listChunk chunk
        & ix n %%~ f
        <&> ListChunk
  Take _ n selector -> do
    compiled <- compileSelectorG cmdF selector
    pure $ listOfFocus compiled >.> liftTrav (taking n traversed)
  TakeEnd _ n selector -> do
    compiled <- compileSelectorG cmdF selector
    pure $ listOfFocus compiled >.> liftTrav (takingEnd n)
  Drop _ n selector -> do
    compiled <- compileSelectorG cmdF selector
    pure $ listOfFocus compiled >.> liftTrav (dropping n traversed)
  DropEnd _ n selector -> do
    compiled <- compileSelectorG cmdF selector
    pure $ listOfFocus compiled >.> liftTrav (droppingEnd n)
  Reversed _ inner -> do
    innerFocus <- compileSelectorG cmdF inner
    let revFocus :: Focus cmd [Chunk] Chunk
        revFocus = case cmdF of
          ViewF -> ViewFocus $ \f chunks -> do
            foldMapM f (reverse chunks)
          ModifyF -> ModifyFocus $ \f chunks -> do
            for (reverse chunks) f
    pure $ listOfFocus innerFocus >.> revFocus
  Contains _ needle -> do
    pure $ liftTrav $ \f chunk ->
      do
        Text.splitOn needle (textChunk chunk)
        & fmap Left
        & List.intersperse (Right needle)
        & traverse \case
          Left txt -> pure (TextChunk txt)
          Right txt -> f (TextChunk txt)
        & fmap (TextChunk . Text.concat . fmap textChunk)
  Action _ expr -> case cmdF of
    ViewF -> compileExpr expr
    ModifyF -> viewInModify <$> compileExpr expr
  ParseJSON pos -> do
    let fwd :: (Focusable m) => Chunk -> m Chunk
        fwd chunk =
          let txt = textChunk chunk
           in case Aeson.eitherDecodeStrict' (Text.encodeUtf8 txt) of
                Left err -> do
                  mayErr $ JsonParseError pos txt (Text.pack err)
                  pure chunk
                Right val -> pure (JsonChunk val)
        bwd :: (Focusable m) => Chunk -> m Chunk
        bwd chunk = pure $ TextChunk . TL.toStrict . TL.decodeUtf8 $ Aeson.encode (jsonChunk chunk)
    pure $ liftSimple fwd bwd
  Cast _pos -> pure $ focusId
  Empty _pos -> pure $ focusEmpty
  Prompt _pos ->
    pure $
      liftSimple
        ( \p -> do
            liftIO $ Text.putStrLn (renderChunk p)
            liftIO $ Text.getLine <&> TextChunk
        )
        pure
  File _pos fileSelector -> do
    let sep :: Text
        sep = "\n"
    fp <- compileSelectorG ViewF fileSelector
    pure $ case cmdF of
      ViewF -> do
        ViewFocus $ \f inp -> do
          inp & getViewFocus fp \path -> do
            let foc = getViewFocus (streamFile sep (Text.unpack $ textChunk path) cmdF)
            foc f ()
      ModifyF -> do
        ModifyFocus $ \f inp -> do
          inp & getViewFocus fp \path -> do
            let foc = getModifyFocus (streamFile sep (Text.unpack $ textChunk path) cmdF)
            foc f ()
          pure UnitChunk
  DebugTrace _pos labelSel -> do
    lbl <- compileSelectorG ViewF labelSel
    let fwd :: forall m. (Focusable m) => Chunk -> m Chunk
        fwd chunk = do
          chunk & getViewFocus lbl \l -> do
            liftIO $ Text.hPutStrLn UnliftIO.stdout $ renderChunk l <> ": " <> renderChunk chunk
          pure chunk
    pure $ liftSimple fwd pure
  where
    hasMatches :: (Focusable m) => CommandF cmd -> Focus cmd i o -> i -> m Bool
    hasMatches cmd foc i = do
      case cmd of
        ViewF {} -> isNothing <$> runMaybeT (forOf (getViewFocus foc) i (\_ -> empty @_ @()))
        ModifyF {} -> isNothing <$> runMaybeT (forOf (getModifyFocus foc) i (\_ -> empty))

    takingEnd :: Int -> Traversal' [a] a
    takingEnd n f xs = do
      let len = length xs
      let (before, after) = splitAt (len - n) xs
      (before <>) <$> traverse f after

    droppingEnd :: Int -> Traversal' [a] a
    droppingEnd n f xs = do
      let len = length xs
      let (before, after) = splitAt (len - n) xs
      (<> after) <$> traverse f before

    modifyRegex :: RE.Regex -> (Traversal' RE.Match Text) -> Focus 'ModifyT Chunk Chunk
    modifyRegex pat trav = ModifyFocus \f chunk -> do
      let txt = textChunk chunk
      TextChunk <$> forOf (RE.regexing pat) txt \match -> do
        let groups =
              match ^. RE.namedGroups
                & Map.mapKeys BindingName
                <&> TextChunk
        Debug.debugM "Match Groups" groups
        local (over focusBindings (Map.union groups)) $ forOf trav match (fmap textChunk . f . TextChunk)

viewRegex :: (HasCallStack) => RE.Regex -> (Traversal' RE.Match Text) -> Focus 'ViewT Chunk Chunk
viewRegex pat trav = ViewFocus \f chunk -> do
  let txt = textChunk chunk
  txt & foldMapMOf (RE.regexing pat) \match -> do
    let groups =
          match ^. RE.namedGroups
            & Map.mapKeys BindingName
            <&> TextChunk
    local (over focusBindings (Map.union groups)) $ foldMapMOf trav (f . TextChunk) match

compilePatternG :: Pattern Pos -> IO (Focus ViewT Chunk Chunk)
compilePatternG = \case
  BindingPattern _p bs -> do
    pure $ ViewFocus \f inp -> do
      local (over focusBindings (Map.insert bs inp)) $ f inp
  PatternString _p _bd pat -> do
    pure $ viewRegex pat RE.match
  PatternList _p pats -> do
    patFocs <- for pats compilePatternG
    pure $ ViewFocus \f inp -> do
      case sequenceA (alignWith zipper patFocs (listChunk inp)) of
        Left () -> pure mempty
        Right matched -> do
          -- Bind all the patterns, then run the continuation inside the bound scope.
          bindPatterns (f inp) matched
    where
      -- Mismatched patterns just fail with the original input.
      zipper (This {}) = Left ()
      zipper (That {}) = Left ()
      zipper (These pat chunk) = Right (pat, chunk)
      bindPatterns m = \case
        [] -> m
        (pat, chunk) : rest -> do
          chunk & getViewFocus pat \_ -> do
            bindPatterns m rest

streamFile :: Text -> FilePath -> CommandF cmd -> Focus cmd () Chunk
streamFile _sep input = \case
  ViewF -> ViewFocus $ \f () -> do
    IO.withFile input UnliftIO.ReadMode \h -> do
      liftIO $ UnliftIO.hSetBuffering h UnliftIO.LineBuffering
      let go = do
            done <- liftIO $ UnliftIO.hIsEOF h
            if done
              then pure mempty
              else do
                line <- liftIO $ Text.hGetLine h
                result <- f (TextChunk $ line)
                (result <>) <$> go
      go
  ModifyF -> ModifyFocus $ \f () -> do
    IO.withFile input UnliftIO.ReadMode \h -> do
      IO.withSystemTempFile "focus.txt" \tempPath tempHandle -> do
        UnliftIO.hSetBuffering h UnliftIO.LineBuffering
        let go = do
              done <- UnliftIO.hIsEOF h
              if done
                then pure ()
                else do
                  line <- liftIO $ Text.hGetLine h
                  result <- textChunk <$> f (TextChunk $ line)
                  liftIO $ Text.hPutStrLn tempHandle result
                  go
        go
        UnliftIO.renameFile tempPath input

-- | Get the first result of a view, or return the original input if no output is
-- produced.
viewFirst :: (Focusable m) => Focus 'ViewT i i -> i -> m i
viewFirst (ViewFocus f) i = do
  f (pure . First . Just) i >>= \case
    First Nothing -> pure i
    First (Just x) -> pure x

-- | Expressions aren't reversible and are always ViewT, but it's still often useful to have
-- them inside two-way selectors. It's the typechecker's job to ensure all expressions return
-- a valid result, or to at least warn the user if it's possible for an expression to fail
-- entirely. In those cases the input is returned instead of a result (which usually isn't
-- what the user intended, hence the typechecker warnings)
viewInModify :: Focus ViewT Chunk Chunk -> Focus ModifyT Chunk Chunk
viewInModify = \case
  ViewFocus f -> ModifyFocus $ \g chunk -> do
    r <-
      chunk & f \foc -> do
        fmap (First . Just) $ g foc
    case getFirst r of
      Nothing -> pure chunk
      Just x -> pure x

mayErr :: (Focusable m) => SelectorError -> m ()
mayErr err = do
  handler <- view (focusOpts . handleErr)
  liftIO (handler err)

-- resolveBindingString :: (Focusable m) => Chunk -> TemplateString expr pos -> m Text
-- resolveBindingString input (TemplateString xs) = do
--   Text.concat <$> for xs \case
--     Left (binding, pos) -> do
--       textChunk <$> resolveBinding pos input binding
--     Right txt -> pure txt

resolveBinding :: (Focusable m) => Pos -> Chunk -> BindingName -> m Chunk
resolveBinding pos input bn@(BindingName name) = do
  bindings <- view focusBindings
  case Map.lookup bn bindings of
    Just chunk -> pure chunk
    Nothing -> do
      Debug.debugM "Bindings before err" bindings
      mayErr $ BindingError pos $ "Binding not in scope: " <> name
      pure $ input

compileRecord :: (Map BindingName (Selector Pos)) -> IO (Focus ViewT Chunk Chunk)
compileRecord fields = do
  fv <- emitZipped fields
  pure $ ViewFocus \f chunk -> do
    let foc = getViewFocus fv
    chunk & foc \rec -> do
      local (over focusBindings (Map.union rec)) $ f (RecordChunk rec)

emitZipped :: forall f. (Traversable f) => (f (Selector Pos)) -> IO (Focus ViewT Chunk (f Chunk))
emitZipped fields = do
  fieldFocuses <- traverse (compileSelectorG ViewF) fields
  let foc :: forall m r. (Focusable m, Monoid r) => (f Chunk -> m r) -> Chunk -> m r
      foc f chunk = do
        let corts :: f (Coroutine (Co.Yield Chunk) m ())
            corts =
              fieldFocuses <&> \fieldFocus ->
                let goF = getViewFocus fieldFocus
                    cort :: Coroutine (Co.Yield Chunk) m ()
                    cort =
                      chunk & goF Co.yield
                 in cort
        let loop :: f (Coroutine (Co.Yield Chunk) m ()) -> m r
            loop xs = do
              step <- do
                for xs \cort -> do
                  Co.resume cort >>= \case
                    Left (Co.Yield req k) ->
                      pure $ (Just req, Just k)
                    Right _ -> pure $ (Nothing, Nothing)
              case traverse fst step of
                Nothing -> pure mempty
                Just resultMap -> do
                  result <- f resultMap
                  case sequenceA (snd <$> step) of
                    Nothing -> pure result
                    Just ks -> (result <>) <$> (loop ks)
        loop corts
  pure $ ViewFocus foc

emitProduct :: [(Selector Pos)] -> IO (Focus ViewT Chunk [Chunk])
emitProduct fields = do
  fieldFocuses <- traverse (compileSelectorG ViewF) fields
  pure $ ViewFocus \f inp -> do
    helper id f inp fieldFocuses
  where
    helper dlist f inp = \case
      [] -> f $ dlist []
      (fieldFocus : rest) -> do
        inp & getViewFocus fieldFocus \field -> do
          helper (dlist . (field :)) f inp rest

condBranches :: [(Selector Pos, Selector Pos)] -> IO (Focus ViewT Chunk Chunk)
condBranches branches = do
  focs <- branches & traversed . both %%~ compileSelectorG ViewF
  pure $ ViewFocus \f inp -> condBranchesRec f inp focs

condBranchesRec :: forall m r. (Focusable m, Monoid r) => (Chunk -> m r) -> Chunk -> [(Focus ViewT Chunk Chunk, Focus ViewT Chunk Chunk)] -> m r
condBranchesRec f inp =
  \case
    [] -> pure mempty
    (condFoc, bodyFoc) : rest -> do
      (r, Monoid.Any committed) <-
        inp
          & getViewFocus
            condFoc
            ( \condChunk -> do
                Writer.tell (Monoid.Any True)
                lift $ getViewFocus bodyFoc f condChunk
            )
          & Writer.runWriterT
      if committed
        then pure r
        else condBranchesRec f inp rest
