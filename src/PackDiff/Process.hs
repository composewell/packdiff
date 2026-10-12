-- |
-- Module      : PackDiff.Process
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Running external programs (@git@, @cabal@, @ghc@).
--
-- The standard output of packdiff carries the diff, so the output of the
-- programs that is shown to the user goes to the standard error.
--
-- Ctrl-C is delivered to the child process only ('interruptChildOnly'); the
-- process library raises 'UserInterrupt' in packdiff after the child has
-- exited. This way packdiff waits for the child before cleaning up, e.g.
-- before restoring a branch while an interrupted @git checkout@ still holds
-- @.git/index.lock@.

module PackDiff.Process
    ( Cmd (..)
    , showCmd
    , readCmd
    , readCmdLines
    , runCmd
    ) where

import Control.Exception (catch, throwIO)
import Data.Function ((&))
import Data.Word (Word8)
import Streamly.Data.Array (Array)
import Streamly.Data.Stream (Stream)
import Streamly.Internal.System.Process (ProcessFailure(..))
import System.IO (hFlush, hPutStrLn, stderr)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.FileSystem.Handle as Handle
import qualified Streamly.Internal.System.Process as Process
import qualified Streamly.Unicode.Stream as Unicode

import PackDiff.Error (PackDiffError(..))

-- | A program, its arguments and the directory to run it in.
data Cmd = Cmd
    { cmdDir :: Maybe FilePath
    , cmdProgram :: String
    , cmdArgs :: [String]
    } deriving (Show)

-- | The command line, for messages.
showCmd :: Cmd -> String
showCmd c = unwords (map quote (cmdProgram c : cmdArgs c)) ++ dir

    where

    quote s
        | null s || any (`elem` " \"'") s = show s
        | otherwise = s
    dir = maybe "" (\d -> " (in " ++ d ++ ")") (cmdDir c)

-- XXX The process stream of streamly-process does not wait for the child to
-- exit when the stream is aborted by an exception other than an interrupt
-- delivered to the child, e.g. SIGTERM: the child is sent SIGTERM and reaped
-- asynchronously. The cleanup may then run while the child is still running
-- (see dev/upstream-fixes.md).
stdoutChunks :: Cmd -> Stream IO (Array Word8)
stdoutChunks c =
    Process.toChunksWith
        (Process.setCwd (cmdDir c) . Process.interruptChildOnly True)
        (cmdProgram c)
        (cmdArgs c)

-- | Turn a process failure into an 'ObtainError' naming the command.
--
-- XXX ProcessFailure carries only the exit code (see dev/upstream-fixes.md).
withFailure :: Cmd -> IO a -> IO a
withFailure c action = action `catch` handler

    where

    handler (ProcessFailure code) =
        throwIO $ ObtainError $
            "command failed with exit code " ++ show code ++ ": " ++ showCmd c

-- | Run a command and return its standard output. Its standard error goes to
-- the standard error of packdiff.
readCmd :: Cmd -> IO String
readCmd c =
    withFailure c
        $ stdoutChunks c
            & Unicode.decodeUtf8Chunks
            & Stream.fold Fold.toList

-- | 'readCmd' split into lines.
readCmdLines :: Cmd -> IO [String]
readCmdLines = fmap lines . readCmd

-- | Run a command and copy its standard output to the standard error of
-- packdiff. The command line is printed first. Returns the standard output.
runCmd :: Cmd -> IO String
runCmd c = do
    hPutStrLn stderr ("packdiff: " ++ showCmd c)
    hFlush stderr
    withFailure c
        $ stdoutChunks c
            & Stream.trace (Handle.putChunk stderr)
            & Unicode.decodeUtf8Chunks
            & Stream.fold Fold.toList
