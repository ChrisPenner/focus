{-# LANGUAGE DataKinds #-}

module Focus.Exec (runView) where

import Control.Monad.Reader
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Focus.Cli (ChunkSize (..))
import Focus.Command
import Focus.Compile (Focus (..), FocusM)
import Focus.Prelude
import Focus.Untyped
import System.IO qualified as IO
import UnliftIO (BufferMode (LineBuffering), Handle, hSetBuffering)

runGeneric :: ChunkSize -> Handle -> Handle -> (Text -> FocusM (Maybe Text)) -> FocusM ()
runGeneric chunkSize input output f = do
  case chunkSize of
    EntireChunks -> do
      chunk <- liftIO (Text.hGetContents input)
      out <- f chunk
      for_ out $ liftIO . Text.hPutStrLn output
    LineChunks -> do
      liftIO $ hSetBuffering input LineBuffering
      liftIO $ hSetBuffering output LineBuffering
      let go = do
            done <- liftIO $ IO.hIsEOF input
            if done
              then pure ()
              else do
                line <- liftIO $ Text.hGetLine input
                result <- f line
                for_ result $ liftIO . Text.hPutStrLn output
                go
      go

runView :: Focus ViewT Chunk Chunk -> ChunkSize -> Handle -> Handle -> FocusM ()
runView (ViewFocus f) chunkSize input output = do
  runGeneric chunkSize input output \chunk -> do
    Nothing <$ f (liftIO . Text.hPutStrLn output . renderChunk) (TextChunk chunk)
