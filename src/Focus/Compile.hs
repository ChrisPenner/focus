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
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.RWS.CPS (MonadReader (..))
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Error.Diagnose qualified as D
import Focus.Command (CommandF (..), CommandT (..), IsCmd)
import Focus.Focus
import Focus.Prelude
import Focus.Types
import Focus.Untyped
import System.Exit (ExitCode (..))
import UnliftIO qualified
import UnliftIO.Process qualified as UnliftIO
import Prelude hiding (reads)

compileSelector :: forall cmd. (IsCmd cmd) => CommandF cmd -> TaggedSelector -> Focus cmd Chunk Chunk
compileSelector cmdF = compileSelectorG absurdF cmdF

compileAction :: TaggedAction -> Focus ViewT Chunk Chunk
compileAction = compileSelectorG compileExpr ViewF

compileExpr :: TaggedExpr -> Focus ViewT Chunk Chunk
compileExpr = \case
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
  Intersperse _pos _actions -> do
    error "Intersperse"

compileSelectorG :: forall cmd expr. (IsCmd cmd) => (expr D.Position -> Focus cmd Chunk Chunk) -> CommandF cmd -> Selector expr D.Position -> Focus cmd Chunk Chunk
compileSelectorG goExpr cmdF = \case
  Compose _ xs -> foldr1 (>.>) (compileSelectorG goExpr cmdF <$> xs)
  SplitFields _ delim -> underText $ liftTrav (\f txt -> (Text.intercalate delim) <$> traverse f (Text.splitOn delim txt))
  SplitLines _ -> underText $ liftTrav (\f txt -> Text.unlines <$> traverse f (Text.lines txt))
  SplitWords _ -> underText $ liftTrav $ (\f txt -> Text.unwords <$> traverse f (Text.words txt))
  Regex _ pat _ -> do
    case cmdF of
      ViewF -> viewRegex pat \f match -> do f (TextChunk $ match ^. RE.match)
      ModifyF -> do modifyRegex pat RE.match
  RegexMatches _ ->
    liftTrav $ _RegexMatchChunk . RE.match . from textI
  RegexGroups _ pat _ -> do
    case cmdF of
      ViewF -> viewRegex pat \f match -> do foldMapM (f . TextChunk) (match ^.. RE.groups . traversed)
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
    let go :: forall x m. (Focusable m) => (Chunk -> m x) -> Chunk -> m x
        go f chunk = do
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
              throwError $ ShellError $ "Shell script failed with exit code " <> tShow code <> ": " <> renderBindingString shellScript <> "\n" <> errOut
            Right out -> do
              f (TextChunk out)
    case cmdF of
      ViewF -> ViewFocus \f chunk -> go f chunk
      ModifyF -> ModifyFocus \f chunk -> go f chunk
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
    viewRegex :: RE.Regex -> (forall m r. (Monoid r, Focusable m) => (Chunk -> m r) -> RE.Match -> m r) -> Focus 'ViewT Chunk Chunk
    viewRegex pat go = ViewFocus \f chunk -> do
      let txt = textChunk chunk
      txt & foldMapMOf (RE.regexing pat) \match -> do
        let groups =
              match ^. RE.namedGroups
                <&> TextChunk
        local (groups <>) $ go f match

    modifyRegex :: RE.Regex -> (Traversal' RE.Match Text) -> Focus 'ModifyT Chunk Chunk
    modifyRegex pat trav = ModifyFocus \f chunk -> do
      let txt = textChunk chunk
      TextChunk <$> forOf (RE.regexing pat) txt \match -> do
        let groups =
              match ^. RE.namedGroups
                <&> TextChunk
        local (groups <>) $ forOf trav match (fmap textChunk . f . TextChunk)

resolveBindingString :: (MonadReader Bindings m, MonadError SelectorError m) => Chunk -> BindingString -> m Text
resolveBindingString input (BindingString xs) = do
  Text.concat <$> for xs \case
    Left (binding, pos) -> do
      textChunk <$> resolveBinding pos input binding
    Right txt -> pure txt

resolveBinding :: (MonadReader Bindings m, MonadError SelectorError m) => Pos -> Chunk -> BindingName -> m Chunk
resolveBinding pos input = \case
  BindingName name -> do
    bindings <- ask
    case Map.lookup name bindings of
      Just chunk -> pure chunk
      Nothing -> throwError $ BindingError pos $ "Binding not in scope: " <> name
  InputBinding -> pure input
