{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Focus.Compile
  ( compileSelector,
    Focus (..),
    textChunk,
    listChunk,
  )
where

import Control.Lens
import Control.Lens.Regex.Text qualified as RE
import Control.Lens.Regex.Text qualified as Re
import Control.Monad.Fix (MonadFix (..))
import Control.Monad.RWS.CPS (MonadWriter (..))
import Control.Monad.State.Lazy
import Control.Monad.Writer (WriterT, execWriterT)
import Data.Maybe (fromMaybe)
import Data.Monoid (Ap (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Vector.Internal.Check (HasCallStack)
import Focus.AST
import Focus.Command (CommandF (..), CommandT (..))
import Focus.Types
import System.Exit (ExitCode (..))
import UnliftIO qualified
import UnliftIO.Process qualified as UnliftIO
import Prelude hiding (reads)

-- data Err = TypeMismatch (ChunkType {- expected -}) (ChunkType {- actual -})

data Focus (cmd :: CommandT) m where
  ViewFocus :: ((Chunk -> m ()) -> Chunk -> m ()) -> Focus 'ViewT m
  ModifyFocus :: LensLike' m Chunk Chunk -> Focus 'ModifyT m
  SetFocus :: LensLike' m Chunk Chunk -> Focus 'SetT m

getViewFocus :: Focus 'ViewT m -> ((Chunk -> m ()) -> Chunk -> m ())
getViewFocus (ViewFocus f) = f

getModifyFocus :: Focus 'ModifyT m -> LensLike' m Chunk Chunk
getModifyFocus (ModifyFocus f) = f

textChunk :: (HasCallStack) => Chunk -> Text
textChunk = \case
  TextChunk txt -> txt
  actual -> error $ "Expected TextChunk, got " <> show actual

listChunk :: Chunk -> [Chunk]
listChunk = \case
  ListChunk chs -> chs
  actual -> error $ "Expected ListChunk, got " <> show actual

-- compileSelector :: (MonadIO m, MonadFix m) => CommandF cmd -> Selector -> Focus cmd m
-- compileSelector cmdF = \case
--   Compose selectors ->
--     foldr1 composeFT (compileSelector cmdF <$> selectors)

composeFT :: Focus cmd m -> Focus cmd m -> Focus cmd m
composeFT (ViewFocus l) (ViewFocus r) = ViewFocus $ l . r
composeFT (ModifyFocus l) (ModifyFocus r) = ModifyFocus $ l . r
composeFT (SetFocus l) (SetFocus r) = SetFocus $ l . r

liftTrav :: (Applicative m) => CommandF cmd -> Traversal' Chunk Chunk -> Focus cmd m
liftTrav cmdF trav = case cmdF of
  ViewF -> ViewFocus $ \handler chunk -> getAp $ foldMapOf trav (Ap . handler) chunk
  ModifyF -> ModifyFocus $ trav

textI :: Iso' Chunk Text
textI = unsafeIso _TextChunk

matchI :: Iso' Chunk Re.Match
matchI = unsafeIso _RegexMatchChunk

underText :: Traversal' Text Text -> Traversal' Chunk Chunk
underText = withinIso textI

withinIso :: Iso' s a -> Traversal' a a -> Traversal' s s
withinIso i t = i . t . from i

unsafeIso :: (Show s) => Prism' s a -> Iso' s a
unsafeIso p = iso (\actual -> fromJust actual . preview p $ actual) (review p)
  where
    fromJust actual = \case
      Just x -> x
      Nothing -> error $ "unsafeIso: Mismatch. Actual: " <> show actual

compileSelector :: forall m cmd. (MonadIO m, MonadFix m) => CommandF cmd -> Selector -> Focus cmd m
compileSelector cmdF = \case
  Compose selectors ->
    foldr1 composeFT (compileSelector cmdF <$> selectors)
  SplitFields delim ->
    liftTrav cmdF $ underText (\f txt -> (Text.intercalate delim) <$> traverse f (Text.splitOn delim txt))
  SplitLines ->
    liftTrav cmdF $ underText (\f txt -> Text.unlines <$> traverse f (Text.lines txt))
  SplitWords ->
    liftTrav cmdF $ underText (\f txt -> Text.unwords <$> traverse f (Text.words txt))
  Regex pat -> do
    liftTrav cmdF $ textI . RE.regexing pat . from matchI
  RegexMatches ->
    liftTrav cmdF $ _RegexMatchChunk . RE.match . from textI
  RegexGroups ->
    liftTrav cmdF $ _RegexMatchChunk . RE.groups . traversed . from textI
  ListOf selector -> do
    case cmdF of
      ViewF -> ViewFocus $ \f chunk -> do
        let t :: ((Chunk -> WriterT [Chunk] IO ()) -> Chunk -> WriterT [Chunk] IO ())
            t = getViewFocus $ compileSelector cmdF selector
        results <-
          chunk
            & t
              %%~ ( \chunk' -> do
                      tell [chunk']
                  )
            & execWriterT
            & liftIO
        f (ListChunk $ results)
      ModifyF -> ModifyFocus $ \f chunk -> do
        let inner :: LensLike' (StateT ListOfS IO) Chunk Chunk
            inner = getModifyFocus $ compileSelector cmdF selector
        let action :: StateT ListOfS IO Chunk
            action =
              chunk
                & inner %%~ \innerChunk -> do
                  state \(ListOfS {reads = ~reads, results = ~results}) ->
                    let ~(result, newResults) = case results of
                          ~(hd : rest) -> (hd, rest)
                     in (result, ListOfS {reads = (innerChunk : reads), results = newResults})
        ~(r, _) <- mfix $ \(~(_r, results)) -> do
          ~(r, ListOfS {reads}) <- liftIO $ runStateT action (ListOfS {reads = [], results})
          f (ListChunk $ reverse reads) >>= \case
            ~(ListChunk chs) -> pure $ (r, chs)
        pure r
  Shell shellScript -> do
    let go :: forall x. (Chunk -> m x) -> Chunk -> m x
        go f chunk = do
          let proc = UnliftIO.shell (Text.unpack shellScript)
          let proc' = proc {UnliftIO.std_in = UnliftIO.CreatePipe, UnliftIO.std_out = UnliftIO.CreatePipe}
          out' <- liftIO $ UnliftIO.withCreateProcess proc' \mstdin mstdout _stderr phandle -> do
            let stdin = fromMaybe (error "missing stdin") mstdin
            let stdout = fromMaybe (error "missing stdout") mstdout
            Text.hPutStrLn stdin (textChunk chunk)
            UnliftIO.hClose stdin
            UnliftIO.waitForProcess phandle >>= \case
              ExitSuccess -> pure ()
              ExitFailure code -> error $ "Shell script failed with exit code " <> show code
            out <- Text.hGetContents stdout
            let out' = fromMaybe out $ Text.stripSuffix "\n" out
            pure out'
          f (TextChunk out')
    case cmdF of
      ViewF -> ViewFocus \f chunk -> go f chunk
      ModifyF -> ModifyFocus \f chunk -> go f chunk
  At n -> do
    liftTrav cmdF $ \f chunk -> do
      listChunk chunk
        & ix n %%~ f
        <&> ListChunk

data ListOfS = ListOfS
  { reads :: [Chunk],
    results :: [Chunk]
  }
