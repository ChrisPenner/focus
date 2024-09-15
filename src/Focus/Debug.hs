module Focus.Debug (debugM) where

import Data.Text (Text)
import Data.Text qualified as Text
import Debug.Trace (traceM)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

shouldDebug :: Bool
shouldDebug =
  unsafePerformIO $
    lookupEnv "FOCUS_DEBUG" >>= \case
      Just "true" -> pure True
      _ -> pure False
{-# NOINLINE shouldDebug #-}

debugM :: (Show a, Monad m) => Text -> a -> m ()
debugM msg a
  | shouldDebug = traceM $ (Text.unpack msg <> ": \n") <> show a
  | otherwise = pure ()
