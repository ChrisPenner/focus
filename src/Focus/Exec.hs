{-# LANGUAGE DataKinds #-}

module Focus.Exec (runView, runSet, runModify) where

import Control.Lens
import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Focus.Command
import Focus.Compile (Focus (..), FocusM)
import Focus.Typechecker.Types (Chunk (..))
import System.IO qualified as IO
import UnliftIO (BufferMode (LineBuffering), Handle, hSetBuffering)

runView :: Focus ViewT FocusM -> Handle -> Handle -> FocusM ()
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

runSet :: Focus ModifyT FocusM -> Handle -> Handle -> Text -> FocusM ()
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

runModify :: Focus ModifyT FocusM -> Focus ModifyT FocusM -> Handle -> Handle -> FocusM ()
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
  RegexMatchChunk _m -> error "Can't render a regex match chunk"
