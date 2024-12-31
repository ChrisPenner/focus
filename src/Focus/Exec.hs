{-# LANGUAGE DataKinds #-}

module Focus.Exec (runModify, runAligned) where

import Control.Lens
import Control.Monad.Reader
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Focus.Cli (ChunkSize (..))
import Focus.Command
import Focus.Compile (Focus (..), FocusM)
import Focus.Prelude
import Focus.Types (focusBindings)
import Focus.Untyped
import System.IO qualified as IO
import UnliftIO (BufferMode (LineBuffering), Handle, hSetBuffering)

runGeneric :: ChunkSize -> Maybe Handle -> Handle -> (Chunk -> FocusM (Maybe Text)) -> FocusM ()
runGeneric chunkSize input output f = do
  case chunkSize of
    EntireChunks -> do
      chunk <- case input of
        Nothing -> pure UnitChunk
        Just inputHandle -> do TextChunk <$> liftIO (Text.hGetContents inputHandle)
      out <- f chunk
      for_ out $ liftIO . Text.hPutStrLn output
    LineChunks -> do
      liftIO $ hSetBuffering output LineBuffering
      case input of
        Nothing -> do
          result <- f UnitChunk
          for_ result $ liftIO . Text.hPutStrLn output
        Just inputHandle -> do
          liftIO $ hSetBuffering inputHandle LineBuffering
          let go = do
                done <- liftIO $ IO.hIsEOF inputHandle
                if done
                  then pure ()
                  else do
                    line <- liftIO $ Text.hGetLine inputHandle
                    result <- f $ TextChunk line
                    for_ result $ liftIO . Text.hPutStrLn output
                    go
          go

runAligned :: Focus ViewT Chunk Chunk -> ChunkSize -> [Handle] -> Handle -> FocusM ()
runAligned (ViewFocus f) chunkSize inputs output = do
  runAlignedHelper chunkSize inputs output \chunk -> do
    Nothing <$ f (liftIO . Text.hPutStrLn output . renderChunk) chunk

runAlignedHelper :: ChunkSize -> [Handle] -> Handle -> (Chunk -> FocusM (Maybe Chunk)) -> FocusM ()
runAlignedHelper chunkSize inputs output f = do
  case chunkSize of
    EntireChunks -> do
      chunks <- liftIO (traverse Text.hGetContents inputs)
      out <- withBindings chunks $ f (chunks & fmap TextChunk & ListChunk)
      for_ out $ liftIO . Text.hPutStrLn output . renderChunk
    LineChunks -> do
      liftIO $ for_ inputs \input -> hSetBuffering input LineBuffering
      liftIO $ hSetBuffering output LineBuffering
      let go = do
            results <- for inputs \input -> do
              done <- liftIO $ IO.hIsEOF input
              if done
                then pure Nothing
                else do
                  line <- liftIO $ Text.hGetLine input
                  pure (Just line)
            case (sequenceA results) of
              Nothing ->
                -- At least one input is empty
                pure ()
              Just theLines -> do
                let list = ListChunk (TextChunk <$> theLines)
                mayResult <- withBindings theLines $ f list
                for_ mayResult $ liftIO . Text.hPutStrLn output . renderChunk
                go
      go
  where
    withBindings :: [Text] -> FocusM a -> FocusM a
    withBindings chunks =
      let binds = Map.fromList $ zipWith (\i chunk -> (BindingName $ Text.pack ("f" <> show i), TextChunk chunk)) [(1 :: Int) ..] chunks
       in local (over focusBindings (Map.union binds))

runModify :: Focus ViewT Chunk Chunk -> ChunkSize -> Maybe Handle -> Handle -> FocusM ()
runModify (ViewFocus f) chunkSize input output = do
  runGeneric chunkSize input output \chunk -> do
    Nothing <$ f (liftIO . Text.hPutStrLn output . renderChunk) chunk
