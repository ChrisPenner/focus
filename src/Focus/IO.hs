{-# LANGUAGE CPP #-}

-- Strongly inspired by the <https://github.com/feuerbach/temporary> package.
--
-- === __Copyright notice:__
--
-- The following copyright notice is taken from <https://github.com/feuerbach/temporary>
-- and is reproduced here as part of license terms of that package, of which this module is
-- a derivate work.
--
-- @
-- Copyright
--   (c) 2003-2006, Isaac Jones
--   (c) 2005-2009, Duncan Coutts
--   (c) 2008, Maximilian Bolingbroke
--   ... and other contributors
--
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without modification, are permitted
-- provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright notice, this list of
--       conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright notice, this list of
--       conditions and the following disclaimer in the documentation and/or other materials
--       provided with the distribution.
--     * Neither the name of Maximilian Bolingbroke nor the names of other contributors may be used to
--       endorse or promote products derived from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
-- IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
-- FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
-- CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
-- IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
-- OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-- @
module Focus.IO (withFile, withSystemTempFile) where

import Control.Monad (liftM)
import System.FilePath ((</>))
import System.IO (hClose)
import System.IO.Error
import System.Posix.Internals (c_getpid)
import UnliftIO.Directory qualified as UnliftIO
import UnliftIO.Exception
import UnliftIO.IO qualified as IO
import UnliftIO.Resource (MonadResource)
import UnliftIO.Resource qualified as Res

#ifdef mingw32_HOST_OS
import System.Directory       ( createDirectory )
#else
import qualified System.Posix
import qualified System.IO as SystemIO
#endif

with :: (MonadResource m) => IO a -> (a -> IO ()) -> (a -> m r) -> m r
with acquire release action = do
  (releaseKey, resource) <- Res.allocate acquire release
  r <- action resource
  Res.release releaseKey
  pure r

withFile :: (MonadResource m) => FilePath -> IO.IOMode -> (IO.Handle -> m a) -> m a
withFile fp mode = with (IO.openFile fp mode) IO.hClose

withSystemTempFile :: (MonadResource m) => String -> (FilePath -> IO.Handle -> m a) -> m a
withSystemTempFile template action = do
  with acquire cleanup (\(_dir, fp, h) -> action fp h)
  where
    acquire = do
      targetDir <- UnliftIO.getTemporaryDirectory >>= UnliftIO.canonicalizePath
      tempDir <- createTempDirectory targetDir
      (fname, fhandle) <- SystemIO.openTempFile tempDir template
      pure (tempDir, fname, fhandle)
    createTempDirectory dir = do
      pid <- c_getpid
      findTempName pid
      where
        findTempName x = do
          let dirpath = dir </> template ++ show x
          r <- try $ mkPrivateDir dirpath
          case r of
            Right _ -> return dirpath
            Left e
              | isAlreadyExistsError e -> findTempName (x + 1)
              | otherwise -> ioError e

    cleanup (tempDir, _fname, fhandle) = do
      hClose fhandle
      ignoringIOErrors $ UnliftIO.removeDirectoryRecursive tempDir

ignoringIOErrors :: IO a -> IO ()
ignoringIOErrors = liftM (const ()) . tryIO

mkPrivateDir :: String -> IO ()
#ifdef mingw32_HOST_OS
mkPrivateDir s = createDirectory s
#else
mkPrivateDir s = System.Posix.createDirectory s 0o700
#endif

--       IO.withSystemTempFile "focus.txt" \tempPath tempHandle -> do
--         UnliftIO.hSetBuffering h UnliftIO.LineBuffering
--         let go = do
--               done <- UnliftIO.hIsEOF h
--               if done
--                 then pure ()
--                 else do
--                   line <- Text.hGetLine h
--                   result <- textChunk <$> f (TextChunk $ line)
--                   liftIO $ Text.hPutStrLn tempHandle result
--                   go
--         go
--         UnliftIO.renameFile tempPath input
