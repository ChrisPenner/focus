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

import Control.Lens
import Control.Lens.Regex.Text qualified as RE
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.RWS.CPS (MonadReader (..))
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Error.Diagnose qualified as D
import Focus.Command (CommandF (..), CommandT (..))
import Focus.Focus
import Focus.Prelude
import Focus.Typechecker.Types qualified as T
import Focus.Types
import Focus.Untyped
import System.Exit (ExitCode (..))
import UnliftIO qualified
import UnliftIO.Process qualified as UnliftIO
import Prelude hiding (reads)

compileSelector :: forall cmd i o. CommandF cmd -> TypedSelector i o (D.Position) -> Focus cmd Chunk Chunk
compileSelector cmdF = \case
  T.Compose _ l r ->
    let l' = compileSelector cmdF l
        r' = compileSelector cmdF r
     in l' >.> r'
  T.SplitFields _ delim ->
    liftTrav cmdF $ underText (\f txt -> (Text.intercalate delim) <$> traverse f (Text.splitOn delim txt))
  T.SplitLines _ ->
    liftTrav cmdF $ underText (\f txt -> Text.unlines <$> traverse f (Text.lines txt))
  T.SplitWords _ ->
    liftTrav cmdF $ underText (\f txt -> Text.unwords <$> traverse f (Text.words txt))
  T.Regex _ pat -> do
    case cmdF of
      ViewF -> viewRegex pat \f match -> do f (TextChunk $ match ^. RE.match)
      ModifyF -> do modifyRegex pat RE.match
  T.RegexMatches _ ->
    liftTrav cmdF $ _RegexMatchChunk . RE.match . from textI
  T.RegexGroups _ pat -> do
    case cmdF of
      ViewF -> viewRegex pat \f match -> do traverse_ (f . TextChunk) (match ^.. RE.groups . traversed)
      ModifyF -> do modifyRegex pat (RE.groups . traverse)
  T.ListOf _ selector -> do
    listOfFocus (compileSelector cmdF selector) >.> liftTrav cmdF (from asListI)
  T.FilterBy _ selector -> do
    case cmdF of
      ViewF -> do
        let inner :: (Focusable m) => (Chunk -> MaybeT m ()) -> Chunk -> MaybeT m ()
            inner = getViewFocus $ compileSelector cmdF selector
        ViewFocus $ \f chunk -> do
          runMaybeT (forOf inner chunk (\_ -> empty)) >>= \case
            -- Nothing means we got at least one result and short-circuted.
            Nothing -> f chunk
            -- Just means we didn't get any results, so the computation succeeded.
            Just _ -> pure ()
      ModifyF -> ModifyFocus $ \f chunk -> do
        let inner :: (Focusable m) => LensLike' (MaybeT m) Chunk Chunk
            inner = getModifyFocus $ compileSelector cmdF selector
        runMaybeT (forOf inner chunk (\_ -> empty)) >>= \case
          -- Nothing means we got at least one result and short-circuted.
          Nothing -> f chunk
          -- Just means we didn't get any results, so the computation succeeded.
          Just _ -> pure chunk
  T.Splat _ -> do
    liftTrav cmdF $ \f chunk -> do
      listChunk chunk
        & traversed %%~ f
        <&> ListChunk
  T.Shell _ shellScript shellMode -> do
    let go :: forall x m. (Focusable m) => (Chunk -> m x) -> Chunk -> m x
        go f chunk = do
          shellTxt <- resolveBindingString shellScript
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
  T.At _ n -> do
    liftTrav cmdF $ \f chunk -> do
      listChunk chunk
        & ix n %%~ f
        <&> ListChunk
  T.Take _ n selector -> do _
  T.Drop _ n selector -> do _
  where
    viewRegex :: RE.Regex -> (forall m. (Focusable m) => (Chunk -> m ()) -> RE.Match -> m ()) -> Focus 'ViewT Chunk Chunk
    viewRegex pat go = ViewFocus \f chunk -> do
      let txt = textChunk chunk
      forOf_ (RE.regexing pat) txt \match -> do
        let groups =
              match ^. RE.namedGroups
                & Map.mapKeys BindingName
                <&> TextChunk
        local (groups <>) $ go f match

    modifyRegex :: RE.Regex -> (Traversal' RE.Match Text) -> Focus 'ModifyT Chunk Chunk
    modifyRegex pat trav = ModifyFocus \f chunk -> do
      let txt = textChunk chunk
      TextChunk <$> forOf (RE.regexing pat) txt \match -> do
        let groups =
              match ^. RE.namedGroups
                & Map.mapKeys BindingName
                <&> TextChunk
        local (groups <>) $ forOf trav match (fmap textChunk . f . TextChunk)

resolveBindingString :: (MonadReader Bindings m, MonadError SelectorError m) => BindingString -> m Text
resolveBindingString (BindingString xs) = do
  bindings <- ask
  Text.concat <$> for xs \case
    Left (name@(BindingName nameTxt), pos) -> do
      case Map.lookup name bindings of
        Just chunk -> pure $ textChunk chunk
        Nothing -> throwError $ BindingError pos $ "Binding not found: " <> nameTxt
    Right txt -> pure txt

data ListOfS = ListOfS
  { reads :: [Chunk],
    results :: [Chunk]
  }
