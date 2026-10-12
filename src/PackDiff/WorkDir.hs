{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : PackDiff.WorkDir
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- The work directory holds work clones and unpacked Hackage sources. Each
-- directory created in it is named @<pid>-<name>@ and deleted when the run
-- ends. A directory left behind by a killed run is deleted by the next run,
-- which finds that its process is no longer running.

module PackDiff.WorkDir
    ( WorkSpace
    , defaultWorkRoot
    , withWorkSpace
    , newWorkDir
    , currentPid
    , isProcessAlive
    , parsePidPrefix
    , removeTree
    ) where

import Control.Concurrent (threadDelay)
import Control.Exception
    (SomeException, bracket, displayException, try, uninterruptibleMask_)
import Control.Monad (forM_, unless, when)
import Data.Char (isDigit)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import System.Directory (XdgDirectory(..), doesPathExist, getXdgDirectory)
import System.IO (hPutStrLn, stderr)

import qualified Coreutils.Mkdir as Mkdir
import qualified Coreutils.Rm as Rm
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.FileSystem.DirIO as DirIO
import qualified Streamly.FileSystem.Path as Path

#if defined(mingw32_HOST_OS)
import qualified System.Win32.File as Win32
import qualified System.Win32.Process as Win32
#else
import System.IO.Error (isPermissionError)
import qualified System.Posix.Process as Posix
import qualified System.Posix.Signals as Posix
#endif

--------------------------------------------------------------------------------
-- Processes
--------------------------------------------------------------------------------

currentPid :: IO Int
#if defined(mingw32_HOST_OS)
currentPid = fromIntegral <$> Win32.getCurrentProcessId
#else
currentPid = fromIntegral <$> Posix.getProcessID
#endif

-- | Whether a process with this id is running.
--
-- XXX This and 'currentPid' belong in streamly-coreutils (see
-- dev/upstream-fixes.md).
--
-- XXX On Windows this is not tested. A process that has exited but whose
-- handle is still open somewhere is reported as running.
isProcessAlive :: Int -> IO Bool
#if defined(mingw32_HOST_OS)
isProcessAlive pid = do
    r <- try (Win32.openProcess Win32.pROCESS_QUERY_INFORMATION False (fromIntegral pid))
    case r of
        Left (_ :: SomeException) -> pure False
        Right h -> Win32.closeHandle h >> pure True
#else
isProcessAlive pid = do
    r <- try (Posix.signalProcess Posix.nullSignal (fromIntegral pid))
    case r of
        Right () -> pure True
        -- EPERM: the process exists but belongs to another user
        Left e -> pure (isPermissionError e)
#endif

--------------------------------------------------------------------------------
-- Directories
--------------------------------------------------------------------------------

-- XXX coreutils functions take a streamly Path while the directory and
-- process packages take a FilePath, so paths are converted at each call.
toPath :: FilePath -> Path.Path
toPath = Path.fromString_

-- | Delete a directory tree if it exists.
--
-- XXX A child process aborted by an exception may still be running and
-- writing into the tree, because streamly-process does not wait for it (see
-- dev/upstream-fixes.md), so the deletion is retried a few times.
removeTree :: FilePath -> IO ()
removeTree dir = go (5 :: Int)

    where

    go n = do
        r <- try (Rm.rm (Rm.recursive True . Rm.withForce Rm.Force) (toPath dir))
        case r of
            Right () -> pure ()
            Left (e :: SomeException)
                | n > 1 -> threadDelay 200000 >> go (n - 1)
                | otherwise ->
                    hPutStrLn stderr $
                        "packdiff: could not delete " ++ dir ++ ": "
                            ++ displayException e

-- | The process id at the start of a work directory name, @<pid>-<name>@.
parsePidPrefix :: String -> Maybe Int
parsePidPrefix name =
    case span isDigit name of
        (ds@(_ : _), '-' : _) -> Just (read ds)
        _ -> Nothing

data WorkSpace = WorkSpace
    { wsRoot :: FilePath
    , wsPid :: Int
    , wsDirs :: IORef [FilePath]
    }

-- | @$XDG_CACHE_HOME/packdiff/work@
defaultWorkRoot :: IO FilePath
defaultWorkRoot = fmap (++ "/work") (getXdgDirectory XdgCache "packdiff")

-- | Delete the directories of runs that are no longer running.
removeStale :: FilePath -> Int -> IO ()
removeStale root self = do
    entries <-
        Stream.fold Fold.toList (DirIO.read (toPath root))
    forM_ entries $ \entry -> do
        let name = baseName (Path.toString entry)
        case parsePidPrefix name of
            Just pid | pid /= self -> do
                alive <- isProcessAlive pid
                unless alive $ removeTree (root ++ "/" ++ name)
            _ -> pure ()

    where

    baseName = reverse . takeWhile (\c -> c /= '/' && c /= '\\') . reverse

-- | Run an action with a work space in the given root directory. Directories
-- created with 'newWorkDir' are deleted when the action ends, including on an
-- exception.
withWorkSpace :: FilePath -> (WorkSpace -> IO a) -> IO a
withWorkSpace root action = do
    Mkdir.mkdir (Mkdir.makeParents True) (toPath root)
    pid <- currentPid
    removeStale root pid
    ref <- newIORef []
    bracket
        (pure (WorkSpace root pid ref))
        (\ws -> uninterruptibleMask_ $ readIORef (wsDirs ws) >>= mapM_ removeTree)
        action

-- | Create an empty directory @<root>/<pid>-<name>@ that is deleted when the
-- work space is released.
newWorkDir :: WorkSpace -> String -> IO FilePath
newWorkDir ws name = do
    let dir = wsRoot ws ++ "/" ++ show (wsPid ws) ++ "-" ++ name
    modifyIORef' (wsDirs ws) (dir :)
    exists <- doesPathExist dir
    when exists $ removeTree dir
    Mkdir.mkdir (Mkdir.makeParents True) (toPath dir)
    pure dir
