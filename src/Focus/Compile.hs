{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Focus.Compile
  ( compileAST,
    Focus (..),
    textChunk,
    listChunk,
  )
where

import Control.Lens
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
import Focus.Types (Chunk (..))
import System.Exit (ExitCode (..))
import UnliftIO qualified
import UnliftIO.Process qualified as UnliftIO
import Prelude hiding (reads)

-- data Err = TypeMismatch (ChunkType {- expected -}) (ChunkType {- actual -})

data Focus (cmd :: CommandT) m where
  ViewFocus :: ((Chunk -> m ()) -> Chunk -> m ()) -> Focus 'ViewT m
  OverFocus :: LensLike' m Chunk Chunk -> Focus 'OverT m
  SetFocus :: LensLike' m Chunk Chunk -> Focus 'SetT m

getViewFocus :: Focus 'ViewT m -> ((Chunk -> m ()) -> Chunk -> m ())
getViewFocus (ViewFocus f) = f

getOverFocus :: Focus 'OverT m -> LensLike' m Chunk Chunk
getOverFocus (OverFocus f) = f

textChunk :: (HasCallStack) => Chunk -> Text
textChunk = \case
  TextChunk txt -> txt
  actual -> error $ "Expected TextChunk, got " <> show actual

listChunk :: Chunk -> [Chunk]
listChunk = \case
  ListChunk chs -> chs
  actual -> error $ "Expected ListChunk, got " <> show actual

compileAST :: (MonadIO m, MonadFix m) => CommandF cmd -> AST -> Focus cmd m
compileAST cmdF = \case
  Compose selectors ->
    foldr1 composeFT (compileSelector cmdF <$> selectors)

composeFT :: Focus cmd m -> Focus cmd m -> Focus cmd m
composeFT (ViewFocus l) (ViewFocus r) = ViewFocus $ l . r
composeFT (OverFocus l) (OverFocus r) = OverFocus $ l . r
composeFT (SetFocus l) (SetFocus r) = SetFocus $ l . r

liftTrav :: (Applicative m) => CommandF cmd -> Traversal' Chunk Chunk -> Focus cmd m
liftTrav cmdF trav = case cmdF of
  ViewF -> ViewFocus $ \handler chunk -> getAp $ foldMapOf trav (Ap . handler) chunk
  OverF -> OverFocus $ trav

compileSelector :: forall m cmd. (MonadIO m, MonadFix m) => CommandF cmd -> Selector -> Focus cmd m
compileSelector cmdF = \case
  SplitFields delim ->
    liftTrav cmdF $ \f chunk ->
      traverse f (fmap TextChunk $ Text.splitOn delim (textChunk chunk))
        <&> TextChunk . Text.intercalate delim . fmap textChunk
  SplitLines ->
    liftTrav cmdF $ \f chunk ->
      traverse f (fmap TextChunk $ Text.lines (textChunk chunk))
        <&> TextChunk . Text.unlines . fmap textChunk
  SplitWords ->
    liftTrav cmdF $ \f chunk ->
      traverse f (fmap TextChunk $ Text.words (textChunk chunk))
        <&> TextChunk . Text.unwords . fmap textChunk
  Regex _ -> error "regex is unsupported"
  ListOf selector -> do
    case cmdF of
      ViewF -> ViewFocus $ \f chunk -> do
        let t :: ((Chunk -> WriterT [Chunk] IO ()) -> Chunk -> WriterT [Chunk] IO ())
            t = getViewFocus $ compileAST cmdF selector
        results <-
          chunk
            & t
              %%~ ( \chunk' -> do
                      tell [chunk']
                  )
            & execWriterT
            & liftIO
        f (ListChunk $ results)
      OverF -> OverFocus $ \f chunk -> do
        let inner :: LensLike' (StateT ListOfS IO) Chunk Chunk
            inner = getOverFocus $ compileAST cmdF selector
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
      OverF -> OverFocus \f chunk -> go f chunk
  At n -> do
    liftTrav cmdF $ \f chunk -> do
      listChunk chunk
        & ix n %%~ f
        <&> ListChunk

data ListOfS = ListOfS
  { reads :: [Chunk],
    results :: [Chunk]
  }
