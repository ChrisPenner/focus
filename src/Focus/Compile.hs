{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Focus.Compile
  ( compileSelector,
    compileAction,
    Focus (..),
    FocusM (..),
    textChunk,
    listChunk,
    SelectorError (..),
  )
where

import Control.Lens
import Control.Lens.Regex.Text qualified as RE
import Control.Monad.Reader
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Aeson qualified as Aeson
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Monoid (First (First, getFirst))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Error.Diagnose qualified as D
import Focus.Command (CommandF (..), CommandT (..), IsCmd)
import Focus.Focus
import Focus.Prelude
import Focus.Tagged (Tagged (..))
import Focus.Types
import Focus.Untyped
import System.Exit (ExitCode (..))
import UnliftIO qualified
import UnliftIO.Process qualified as UnliftIO
import Prelude hiding (reads)

compileSelector :: forall cmd. (IsCmd cmd) => CommandF cmd -> TaggedAction -> Focus cmd Chunk Chunk
compileSelector cmdF = compileSelectorG (compileExpr cmdF) cmdF

compileAction :: TaggedAction -> Focus ViewT Chunk Chunk
compileAction = compileSelectorG (compileExpr ViewF) ViewF

compileExpr :: CommandF cmd -> TaggedExpr -> Focus cmd Chunk Chunk
compileExpr cmd expr = case cmd of
  ViewF -> viewFocus expr
  ModifyF -> oneWay $ viewFocus expr
  where
    viewFocus =
      \case
        Binding pos bindingName -> do
          ViewFocus \f inp -> do
            binding <- resolveBinding pos inp bindingName
            f binding
        Str _pos bindingStr ->
          let handler :: forall m b. (Focusable m) => (Chunk -> m b) -> Chunk -> m b
              handler f inp = do
                txt <- resolveBindingString inp bindingStr
                f (TextChunk txt)
           in ViewFocus handler
        Number _pos num -> liftTrav $ \f _ -> f (NumberChunk num)
        StrConcat _pos innerExpr -> do
          let inner = compileAction innerExpr
          let go :: ((Chunk -> m c) -> a -> m b) -> (Chunk -> m c) -> a -> m b
              go vf f inp =
                inp & vf \xs -> do
                  f . TextChunk $ Text.concat (textChunk <$> listChunk xs)
          ViewFocus $ \f inp -> go (getViewFocus inner) f inp
        Intersperse _pos actions -> do
          let actionFocuses = (listOfFocus . compileAction <$> actions)
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
          let l = compileSelectorG (compileExpr ViewF) ViewF a
          let r = compileSelectorG (compileExpr ViewF) ViewF b
          ViewFocus $ \f chunk -> do
            liftA2 (<>) (getViewFocus l f chunk) (getViewFocus r f chunk)
        Count _ selector -> do
          let inner = compileSelectorG (compileExpr ViewF) ViewF selector
          listOfFocus inner >.> focusTo (pure . NumberChunk . IntNumber . length)
        MathBinOp pos operation ls rs -> do
          let l = compileSelectorG (compileExpr ViewF) ViewF ls
          let r = compileSelectorG (compileExpr ViewF) ViewF rs
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

oneWay :: Focus ViewT Chunk Chunk -> Focus ModifyT Chunk Chunk
oneWay = \case
  ViewFocus f -> ModifyFocus $ \g chunk -> do
    r <-
      chunk & f \foc -> do
        fmap (First . Just) $ g foc
    case getFirst r of
      Nothing -> pure chunk
      Just x -> pure x

compileSelectorG :: forall cmd expr. (IsCmd cmd) => (expr D.Position -> Focus cmd Chunk Chunk) -> CommandF cmd -> Selector expr D.Position -> Focus cmd Chunk Chunk
compileSelectorG goExpr cmdF = \case
  Compose _ xs -> foldr1 (>.>) (compileSelectorG goExpr cmdF <$> xs)
  SplitFields _ delim -> underText $ liftTrav (\f txt -> (Text.intercalate delim) <$> traverse f (Text.splitOn delim txt))
  SplitLines _ -> underText $ liftTrav (\f txt -> Text.unlines <$> traverse f (Text.lines txt))
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
    listOfFocus (compileSelectorG goExpr cmdF selector) >.> liftTrav (from asListI)
  Filter _ selector -> do
    let inner :: Focus cmd Chunk Chunk
        inner = compileSelectorG goExpr cmdF selector
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
        inner = compileSelectorG goExpr cmdF selector
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
  Shell _ shellScript shellMode -> do
    let go :: forall m. (Focusable m) => (Chunk -> m Chunk)
        go chunk = do
          shellTxt <- resolveBindingString chunk shellScript
          let proc = UnliftIO.shell (Text.unpack shellTxt)
          let proc' = proc {UnliftIO.std_in = UnliftIO.CreatePipe, UnliftIO.std_out = UnliftIO.CreatePipe, UnliftIO.std_err = UnliftIO.CreatePipe}
          procResult <- liftIO $ UnliftIO.withCreateProcess proc' \mstdin mstdout mstderr phandle -> do
            let stdin = fromMaybe (error "missing stdin") mstdin
            let stdout = fromMaybe (error "missing stdout") mstdout
            let stderr = fromMaybe (error "missing stderr") mstderr
            case shellMode of
              Normal -> liftIO $ Text.hPutStrLn stdin (textChunk chunk)
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
              mayErr $ ShellError $ "Shell script failed with exit code " <> tShow code <> ": " <> renderBindingString shellScript <> "\n" <> errOut
              pure chunk
            Right out -> do
              pure (TextChunk out)
    liftSimple go pure
  At _ n -> do
    liftTrav $ \f chunk -> do
      listChunk chunk
        & ix n %%~ f
        <&> ListChunk
  Take _ n selector ->
    listOfFocus (compileSelectorG goExpr cmdF selector) >.> liftTrav (taking n traversed)
  TakeEnd _ n selector -> do
    listOfFocus (compileSelectorG goExpr cmdF selector) >.> liftTrav (takingEnd n)
  Drop _ n selector -> do
    listOfFocus (compileSelectorG goExpr cmdF selector) >.> liftTrav (dropping n traversed)
  DropEnd _ n selector -> do
    listOfFocus (compileSelectorG goExpr cmdF selector) >.> liftTrav (droppingEnd n)
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
  Action _ expr -> goExpr expr
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
        local (over focusBindings (groups <>)) $ foldMapMOf trav (f . TextChunk) match

    modifyRegex :: RE.Regex -> (Traversal' RE.Match Text) -> Focus 'ModifyT Chunk Chunk
    modifyRegex pat trav = ModifyFocus \f chunk -> do
      let txt = textChunk chunk
      TextChunk <$> forOf (RE.regexing pat) txt \match -> do
        let groups =
              match ^. RE.namedGroups
                <&> TextChunk
        local (over focusBindings (groups <>)) $ forOf trav match (fmap textChunk . f . TextChunk)

mayErr :: (Focusable m) => SelectorError -> m ()
mayErr err = do
  handler <- view (focusOpts . handleErr)
  liftIO (handler err)

resolveBindingString :: (Focusable m) => Chunk -> BindingString -> m Text
resolveBindingString input (BindingString xs) = do
  Text.concat <$> for xs \case
    Left (binding, pos) -> do
      textChunk <$> resolveBinding pos input binding
    Right txt -> pure txt

resolveBinding :: (Focusable m) => Pos -> Chunk -> BindingName -> m Chunk
resolveBinding pos input = \case
  BindingName name -> do
    bindings <- view focusBindings
    case Map.lookup name bindings of
      Just chunk -> pure chunk
      Nothing -> do
        mayErr $ BindingError pos $ "Binding not in scope: " <> name
        pure $ input
  InputBinding -> pure input
