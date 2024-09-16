{-# LANGUAGE DataKinds #-}

module Focus.Exec (runView, runSet, runModify) where

import Control.Lens
import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Focus.Command
import Focus.Compile (Focus (..))
import Focus.Types (Chunk (..))
import System.IO qualified as IO
import UnliftIO (BufferMode (LineBuffering), Handle, hSetBuffering)

runView :: (MonadIO m) => Focus ViewT m -> Handle -> Handle -> m ()
runView (ViewFocus f) input output = do
  liftIO $ hSetBuffering input LineBuffering
  liftIO $ hSetBuffering output LineBuffering
  let go = do
        done <- liftIO $ IO.hIsEOF input
        if done
          then pure ()
          else do
            line <- liftIO $ Text.hGetLine input
            let chunk = TextChunk line
            f (liftIO . Text.hPutStrLn output . renderChunk) chunk
            go
  go

runSet :: (MonadIO m) => Focus ModifyT m -> Handle -> Handle -> Text -> m ()
runSet (ModifyFocus trav) input output val = do
  liftIO $ hSetBuffering input LineBuffering
  liftIO $ hSetBuffering output LineBuffering
  let go = do
        done <- liftIO $ IO.hIsEOF input
        if done
          then pure ()
          else do
            line <- liftIO $ Text.hGetLine input
            let chunk = TextChunk line
            result <- forOf trav chunk \_foc -> pure $ TextChunk val
            liftIO $ Text.hPutStrLn output (renderChunk result)
            go
  go

runModify :: (MonadIO m) => Focus ModifyT m -> Focus ModifyT m -> Handle -> Handle -> m ()
runModify (ModifyFocus trav) (ModifyFocus modifier) input output = do
  liftIO $ hSetBuffering input LineBuffering
  liftIO $ hSetBuffering output LineBuffering
  let go = do
        done <- liftIO $ IO.hIsEOF input
        if done
          then pure ()
          else do
            line <- liftIO $ Text.hGetLine input
            let chunk = TextChunk line
            result <- forOf (trav . modifier) chunk pure
            liftIO $ Text.hPutStrLn output (renderChunk result)
            go
  go

renderChunk :: Chunk -> Text
renderChunk = \case
  TextChunk txt -> txt
  ListChunk chs -> Text.pack . show $ renderChunk <$> chs
  NumberChunk n -> Text.pack (show n)
