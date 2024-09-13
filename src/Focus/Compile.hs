{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Focus.Compile
  ( compileAST,
    Focus (..),
    textChunk,
    listChunk,
  )
where

import Control.Lens
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Focus.AST
import Focus.Command (CommandF (..), CommandT (..))
import Focus.Types (Chunk (..))
import System.Exit (ExitCode (..))
import UnliftIO qualified
import UnliftIO.Process qualified as UnliftIO
import UnliftIO.Process qualified as UnlitIO

-- data Err = TypeMismatch (ChunkType {- expected -}) (ChunkType {- actual -})

data Focus (cmd :: CommandT) where
  ViewFocus :: ((Chunk -> IO ()) -> Chunk -> IO ()) -> Focus 'ViewT
  OverFocus :: LensLike' IO Chunk Chunk -> Focus 'OverT
  SetFocus :: LensLike' IO Chunk Chunk -> Focus 'SetT

textChunk :: Chunk -> Text
textChunk = \case
  TextChunk txt -> txt
  actual -> error $ "Expected TextChunk, got " <> show actual

listChunk :: Chunk -> [Chunk]
listChunk = \case
  ListChunk chs -> chs
  actual -> error $ "Expected ListChunk, got " <> show actual

compileAST :: CommandF cmd -> AST -> Focus cmd
compileAST cmdF = \case
  Compose selectors ->
    foldr1 composeFT (compileSelector cmdF <$> selectors)

composeFT :: Focus cmd -> Focus cmd -> Focus cmd
composeFT (ViewFocus l) (ViewFocus r) = ViewFocus $ l . r
composeFT (OverFocus l) (OverFocus r) = OverFocus $ l . r
composeFT (SetFocus l) (SetFocus r) = SetFocus $ l . r

liftTrav :: CommandF cmd -> Traversal' Chunk Chunk -> Focus cmd
liftTrav cmdF trav = case cmdF of
  ViewF -> ViewFocus $ \handler chunk -> foldMapOf trav handler chunk
  OverF -> OverFocus $ trav
  SetF -> SetFocus $ trav

compileSelector :: CommandF cmd -> Selector -> Focus cmd
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
  ListOf _selector -> do undefined
  -- let inner = (compileAST cmdF selector)
  -- case cmdF of
  --   ViewF -> ViewFocus $ \f chunk ->
  --     _
  --   -- forOf (partsOf (getViewFocus inner)) chunk \chunks ->
  --   --   f (ListChunk chunks)
  --   --     <&> listChunk
  --   OverF -> OverFocus $ \f chunk -> do
  --     let x = partsOf (getOverFocus inner)
  --     undefined & x %%~ _
  --   -- forOf (partsOf (getOverFocus inner)) chunk \chunks -> _
  --   -- f (ListChunk chunks)
  --   --   <&> listChunk
  --   SetF -> SetFocus $ \f chunk ->
  --     forOf (partsOf (compileAST cmdF selector)) chunk \chunks ->
  --       f (ListChunk chunks)
  --         <&> listChunk
  Shell shellScript -> do
    let go :: forall x. (Chunk -> IO x) -> Chunk -> IO x
        go f chunk = do
          let proc = UnlitIO.shell (Text.unpack shellScript)
          let proc' = proc {UnlitIO.std_in = UnlitIO.CreatePipe, UnlitIO.std_out = UnlitIO.CreatePipe}
          UnlitIO.withCreateProcess proc' \mstdin mstdout _stderr phandle -> do
            let stdin = fromMaybe (error "missing stdin") mstdin
            let stdout = fromMaybe (error "missing stdout") mstdout
            Text.hPutStrLn stdin (textChunk chunk)
            UnliftIO.hClose stdin
            UnliftIO.waitForProcess phandle >>= \case
              ExitSuccess -> pure ()
              ExitFailure code -> error $ "Shell script failed with exit code " <> show code
            out <- Text.hGetContents stdout
            let out' = fromMaybe out $ Text.stripSuffix "\n" out
            f (TextChunk out')
    case cmdF of
      ViewF -> ViewFocus \f chunk -> go f chunk
      OverF -> OverFocus \f chunk -> go f chunk
      SetF -> SetFocus \f chunk -> go f chunk

-- do
--   Text.hPutStrLn stdin (textChunk chunk)
--   UnliftIO.hClose stdin
--   UnliftIO.waitForProcess phandle >>= \case
--     ExitSuccess -> pure ()
--     ExitFailure code -> error $ "Shell script failed with exit code " <> show code
--   TextChunk <$> Text.hGetContents stdout
