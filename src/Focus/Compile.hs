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

import Control.Lens hiding (Reversed)
import Control.Lens.Regex.Text qualified as RE
import Control.Monad.Coroutine (Coroutine)
import Control.Monad.Coroutine qualified as Co
import Control.Monad.Coroutine.SuspensionFunctors qualified as Co
import Control.Monad.Reader
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Aeson qualified as Aeson
import Data.Align qualified as Align
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Monoid (First (First, getFirst))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.These (These (..))
import Error.Diagnose qualified as D
import Focus.Command (CommandF (..), CommandT (..), IsCmd)
import Focus.Debug qualified as Debug
import Focus.Focus
import Focus.Prelude
import Focus.Tagged (Tagged (..))
import Focus.Types
import Focus.Untyped
import System.Exit (ExitCode (..))
import UnliftIO qualified
import UnliftIO.Process qualified as UnliftIO
import Prelude hiding (reads)

compileSelector :: forall cmd. (IsCmd cmd) => CommandF cmd -> TaggedSelector -> Focus cmd Chunk Chunk
compileSelector cmdF = compileSelectorG cmdF

compileExpr :: TaggedExpr -> Focus ViewT Chunk Chunk
compileExpr =
  \case
    Binding pos bindingName -> do
      ViewFocus \f inp -> do
        binding <- resolveBinding pos inp bindingName
        f binding
    Str _pos bindingStr ->
      let handler :: forall m r. (Focusable m, Monoid r) => (Chunk -> m r) -> Chunk -> m r
          handler f inp = do
            compileTemplateString inp bindingStr \txt -> do
              f txt
       in ViewFocus handler
    Number _pos num -> liftTrav $ \f _ -> f (NumberChunk num)
    StrConcat _pos innerExpr -> do
      let inner = compileSelector ViewF innerExpr
      let go :: ((Chunk -> m c) -> a -> m b) -> (Chunk -> m c) -> a -> m b
          go vf f inp =
            inp & vf \xs -> do
              f . TextChunk $ Text.concat (textChunk <$> listChunk xs)
      ViewFocus $ \f inp -> go (getViewFocus inner) f inp
    Intersperse _pos actions -> do
      let actionFocuses = (listOfFocus . compileSelector ViewF <$> actions)
      ViewFocus $ \f inp -> do
        chunkResults <- for actionFocuses \af -> do
          inp & getViewFocus af pure
        let go = \case
              [] -> pure mempty
              ([] : rest) -> go rest
              ((x : xs) : rest) -> do
                r <- f x
                (r <>) <$> go (rest ++ [xs])
        go (NE.toList chunkResults)
    Comma _ a b -> do
      let l = compileSelectorG ViewF a
      let r = compileSelectorG ViewF b
      ViewFocus $ \f chunk -> do
        liftA2 (<>) (getViewFocus l f chunk) (getViewFocus r f chunk)
    Count _ selector -> do
      let inner = compileSelectorG ViewF selector
      listOfFocus inner >.> focusTo (pure . NumberChunk . IntNumber . length)
    Shell pos shellScript shellMode -> ViewFocus \f inp -> do
      compileTemplateString inp shellScript \txt -> do
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
      let l = compileSelectorG ViewF ls
      let r = compileSelectorG ViewF rs
      ViewFocus $ \f inp -> do
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
      compileRecord ViewF fields
    Cycle _pos selector -> do
      listOfFocus (compileSelectorG ViewF selector) >.> liftTrav \f xs -> do
        for (cycle xs) f
  where
    compileTemplateString :: (Focusable m, Monoid r) => Chunk -> TemplateString Pos -> (Chunk -> m r) -> m r
    compileTemplateString inp (TemplateString xs) f = compileTemplateStringHelper inp xs f
    compileTemplateStringHelper :: (Focusable m, Monoid r) => Chunk -> [Either (Selector Pos) Text] -> (Chunk -> m r) -> m r
    compileTemplateStringHelper inp xs f = case xs of
      [] -> f (TextChunk "")
      (Left sel : rest) -> do
        case compileSelectorG ViewF sel of
          ViewFocus vf -> do
            inp
              & vf
                ( \t ->
                    compileTemplateStringHelper inp rest \restT ->
                      f (TextChunk $ textChunk t <> textChunk restT)
                )
      Right txt : rest -> do
        compileTemplateStringHelper inp rest (\t -> f . TextChunk $ (txt <> textChunk t))

compileSelectorG :: forall cmd. (IsCmd cmd) => CommandF cmd -> Selector D.Position -> Focus cmd Chunk Chunk
compileSelectorG cmdF = \case
  Compose _ xs -> foldr1 (>.>) (compileSelectorG cmdF <$> xs)
  Modify _pos sel modifier ->
    case cmdF of
      ViewF -> do
        let selF = compileSelectorG ModifyF sel
        let modF = compileSelectorG ViewF modifier
        ViewFocus $ \f inp -> do
          r <-
            inp & getModifyFocus selF \theFocus -> do
              viewFirst modF theFocus
          f r
      ModifyF -> error "Can't use a |= inside of another assignment. This should have been caught during typechecking, please report this error to the developers."
  SplitFields _ delim -> underText $ liftTrav (\f txt -> (Text.intercalate delim) <$> traverse f (Text.splitOn delim txt))
  SplitLines _ -> underText $ liftTrav (\f txt -> Text.unlines <$> traverse f (Text.lines txt))
  Chars _pos -> underText $ liftTrav $ \f txt -> Text.concat <$> traverse f (Text.singleton <$> Text.unpack txt)
  SplitWords _ -> underText $ liftTrav $ (\f txt -> Text.unwords <$> traverse f (Text.words txt))
  Regex _ pat _ -> do
    case cmdF of
      ViewF -> do viewRegex pat RE.match
      ModifyF -> do modifyRegex pat RE.match
  RegexMatches _ ->
    liftTrav $ _RegexMatchChunk . RE.match . from textI
  RegexGroups _ pat _ -> do
    case cmdF of
      ViewF -> do viewRegex pat (RE.groups . traverse)
      ModifyF -> do modifyRegex pat (RE.groups . traverse)
  ListOf _ selector -> do
    listOfFocus (compileSelectorG cmdF selector) >.> liftTrav (from asListI)
  Filter _ selector -> do
    let inner :: Focus cmd Chunk Chunk
        inner = compileSelectorG cmdF selector
    case cmdF of
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
    let inner :: Focus cmd Chunk Chunk
        inner = compileSelectorG cmdF selector
    case cmdF of
      ViewF -> do
        ViewFocus $ \f chunk -> do
          hasMatches cmdF inner chunk >>= \case
            True -> pure mempty
            False -> f chunk
      ModifyF -> ModifyFocus $ \f chunk -> do
        hasMatches cmdF inner chunk >>= \case
          True -> pure chunk
          False -> f chunk
  Splat _ -> do
    liftTrav $ \f chunk -> do
      listChunk chunk
        & traversed %%~ f
        <&> ListChunk
  At _ n -> do
    liftTrav $ \f chunk -> do
      listChunk chunk
        & ix n %%~ f
        <&> ListChunk
  Take _ n selector ->
    listOfFocus (compileSelectorG cmdF selector) >.> liftTrav (taking n traversed)
  TakeEnd _ n selector -> do
    listOfFocus (compileSelectorG cmdF selector) >.> liftTrav (takingEnd n)
  Drop _ n selector -> do
    listOfFocus (compileSelectorG cmdF selector) >.> liftTrav (dropping n traversed)
  DropEnd _ n selector -> do
    listOfFocus (compileSelectorG cmdF selector) >.> liftTrav (droppingEnd n)
  Reversed _ inner -> do
    let innerFocus = compileSelectorG cmdF inner
        revFocus :: Focus cmd [Chunk] Chunk
        revFocus = case cmdF of
          ViewF -> ViewFocus $ \f chunks -> do
            foldMapM f (reverse chunks)
          ModifyF -> ModifyFocus $ \f chunks -> do
            for (reverse chunks) f
     in listOfFocus innerFocus >.> revFocus
  Contains _ needle -> do
    liftTrav $ \f chunk ->
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
    ModifyF -> viewInModify $ compileExpr expr
  BindingAssignment _pos name -> do
    let go :: forall r m. (Focusable m) => (Chunk -> m r) -> Chunk -> m r
        go f chunk = do
          local (over focusBindings (Map.insert name chunk)) $ f chunk
    liftSimpleWithBindings go pure
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
    liftSimple fwd bwd
  Cast _pos -> focusId
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

    viewRegex :: RE.Regex -> (Traversal' RE.Match Text) -> Focus 'ViewT Chunk Chunk
    viewRegex pat trav = ViewFocus \f chunk -> do
      let txt = textChunk chunk
      txt & foldMapMOf (RE.regexing pat) \match -> do
        let groups =
              match ^. RE.namedGroups
                <&> TextChunk
        local (over focusBindings (Map.union groups)) $ foldMapMOf trav (f . TextChunk) match

    modifyRegex :: RE.Regex -> (Traversal' RE.Match Text) -> Focus 'ModifyT Chunk Chunk
    modifyRegex pat trav = ModifyFocus \f chunk -> do
      let txt = textChunk chunk
      TextChunk <$> forOf (RE.regexing pat) txt \match -> do
        let groups =
              match ^. RE.namedGroups
                <&> TextChunk
        Debug.debugM "Match Groups" groups
        local (over focusBindings (Map.union groups)) $ forOf trav match (fmap textChunk . f . TextChunk)

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
resolveBinding pos input = \case
  BindingName name -> do
    bindings <- view focusBindings
    case Map.lookup name bindings of
      Just chunk -> pure chunk
      Nothing -> do
        Debug.debugM "Bindings before err" bindings
        mayErr $ BindingError pos $ "Binding not in scope: " <> name
        pure $ input
  InputBinding -> pure input

compileRecord :: forall cmd. CommandF cmd -> (Map Text (Selector Pos)) -> Focus cmd Chunk Chunk
compileRecord cmdF fields = do
  case cmdF of
    ViewF {} ->
      let fieldFocuses = compileSelectorG cmdF <$> fields
          foc :: forall m r. (Focusable m, Monoid r) => (Chunk -> m r) -> Chunk -> m r
          foc f chunk = do
            let corts :: Map Text (Coroutine (Co.Yield Chunk) m ())
                corts =
                  fieldFocuses <&> \fieldFocus ->
                    let goF = getViewFocus fieldFocus
                        cort :: Coroutine (Co.Yield Chunk) m ()
                        cort =
                          chunk & goF Co.yield
                     in cort
            let loop :: Map Text (Coroutine (Co.Yield Chunk) m ()) -> m r
                loop xs = do
                  step <- do
                    for xs \cort -> do
                      Co.resume cort >>= \case
                        Left (Co.Yield req k) ->
                          pure $ (Just req, Just k)
                        Right _ -> pure $ (Nothing, Nothing)
                  case traverse fst step of
                    Nothing -> pure mempty
                    Just resultMap -> local (over focusBindings (Map.union resultMap)) $ do
                      result <- f . RecordChunk $ resultMap
                      case sequenceA (snd <$> step) of
                        Nothing -> pure result
                        Just ks -> (result <>) <$> (loop ks)
            loop corts
       in ViewFocus foc
    ModifyF {} ->
      let fieldFocuses = compileSelectorG cmdF <$> fields
          foc :: forall m. (Focusable m) => (Chunk -> m Chunk) -> Chunk -> m Chunk
          foc f chunk = do
            let corts :: Map Text (Coroutine (Co.Request Chunk Chunk) m Chunk)
                corts =
                  fieldFocuses <&> \fieldFocus ->
                    let modF = getModifyFocus fieldFocus
                        cort :: Coroutine (Co.Request Chunk Chunk) m Chunk
                        cort =
                          chunk & modF \focChunk -> do
                            Co.request focChunk
                     in cort
            let loop :: Map Text (Coroutine (Co.Request Chunk Chunk) m Chunk) -> m Chunk
                loop xs = do
                  step <- do
                    for xs \cort -> do
                      Co.resume cort >>= \case
                        Left (Co.Request req k) ->
                          pure $ (Just req, Just k)
                        Right r -> pure $ (Just r, Nothing)
                  case traverse fst step of
                    Nothing -> pure chunk
                    Just resultMap -> local (over focusBindings (Map.union resultMap)) $ do
                      result <- f . RecordChunk $ resultMap
                      case sequenceA (snd <$> step) of
                        Nothing -> pure result
                        Just ks -> do
                          let go = \case
                                This _ -> error "compileRecord-modify: Somehow fields were different between iterations. Please report this"
                                That _ -> error "compileRecord-modify: Somehow fields were different between iterations. Please report this"
                                These k r -> k r
                          (loop $ Align.alignWith go ks resultMap) $> result
            loop corts
       in ModifyFocus foc
