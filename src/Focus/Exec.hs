module Focus.Exec (runView, runSet) where

import Control.Lens
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Focus.Compile (Focus (..))
import Focus.Types (Chunk (..))
import System.IO qualified as IO
import UnliftIO (BufferMode (LineBuffering), Handle, hSetBuffering)

runView :: Focus -> Handle -> Handle -> IO ()
runView (Focus trav) input output = do
  hSetBuffering input LineBuffering
  hSetBuffering output LineBuffering
  let go = do
        done <- IO.hIsEOF input
        if done
          then pure ()
          else do
            line <- Text.hGetLine input
            let chunk = TextChunk line
            forOf_ trav chunk \foc -> do
              Text.hPutStrLn output (renderChunk foc)
            go
  go

runSet :: Focus -> Handle -> Handle -> Text -> IO ()
runSet (Focus trav) input output val = do
  hSetBuffering input LineBuffering
  hSetBuffering output LineBuffering
  let go = do
        done <- IO.hIsEOF input
        if done
          then pure ()
          else do
            line <- Text.hGetLine input
            let chunk = TextChunk line
            result <- forOf trav chunk \_foc -> pure $ TextChunk val
            Text.hPutStrLn output (renderChunk result)
            go
  go

renderChunk :: Chunk -> Text
renderChunk = \case
  TextChunk txt -> txt
  ListChunk chs -> Text.unlines (renderChunk <$> chs)
  NumberChunk n -> Text.pack (show n)
