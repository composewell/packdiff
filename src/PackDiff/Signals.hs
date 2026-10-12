{-# LANGUAGE CPP #-}
-- |
-- Module      : PackDiff.Signals
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- The GHC runtime delivers SIGINT (Ctrl-C) as a 'UserInterrupt' exception,
-- but by default exits on SIGTERM and SIGHUP without running any cleanup.
-- The handlers installed here throw 'Terminated' to the main thread so that
-- the cleanup in 'Control.Exception.bracket' runs.
--
-- XXX This is a general facility that belongs in streamly-coreutils (see
-- dev/upstream-fixes.md).

module PackDiff.Signals
    ( Terminated (..)
    , installTerminationHandlers
    ) where

import Control.Concurrent (myThreadId, throwTo)
import Control.Exception (Exception)

#if defined(mingw32_HOST_OS)
import Control.Exception (AsyncException(UserInterrupt))
import qualified GHC.ConsoleHandler as Console
#else
import qualified System.Posix.Signals as Posix
#endif

-- | The process received a termination signal, named by the string.
newtype Terminated = Terminated String
    deriving Show

instance Exception Terminated

-- | Must be called from the main thread.
installTerminationHandlers :: IO ()
#if defined(mingw32_HOST_OS)
installTerminationHandlers = do
    tid <- myThreadId
    let handler ev =
            case ev of
                -- Installing a handler replaces the default Ctrl-C handling.
                Console.ControlC -> throwTo tid UserInterrupt
                Console.Break -> throwTo tid (Terminated "Ctrl-Break")
                Console.Close -> throwTo tid (Terminated "console closed")
                Console.Logoff -> throwTo tid (Terminated "logoff")
                Console.Shutdown -> throwTo tid (Terminated "shutdown")
    _ <- Console.installHandler (Console.Catch handler)
    pure ()
#else
installTerminationHandlers = do
    tid <- myThreadId
    let install sig name =
            Posix.installHandler sig
                (Posix.Catch (throwTo tid (Terminated name))) Nothing
    _ <- install Posix.sigTERM "SIGTERM"
    _ <- install Posix.sigHUP "SIGHUP"
    pure ()
#endif
