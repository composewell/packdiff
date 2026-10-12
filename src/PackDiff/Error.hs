-- |
-- Module      : PackDiff.Error
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Errors and their exit codes.

module PackDiff.Error
    ( PackDiffError (..)
    , usageError
    , obtainError
    , errorExitCode
    , onObtainError
    ) where

import Control.Exception (Exception(..), catch, throwIO)

data PackDiffError
    = UsageError String
      -- ^ Wrong usage, including a missing external program.
    | ObtainError String
      -- ^ Failure to obtain an API: a failed git or cabal command, a build
      -- failure or a hoogle file parse error.
    deriving Show

instance Exception PackDiffError where
    displayException (UsageError msg) = msg
    displayException (ObtainError msg) = msg

usageError :: String -> IO a
usageError = throwIO . UsageError

obtainError :: String -> IO a
obtainError = throwIO . ObtainError

errorExitCode :: PackDiffError -> Int
errorExitCode (UsageError _) = 2
errorExitCode (ObtainError _) = 3

-- | Run the handler if the action throws an 'ObtainError'.
onObtainError :: IO a -> IO a -> IO a
onObtainError action handler =
    action `catch` \e ->
        case e of
            ObtainError _ -> handler
            UsageError _ -> throwIO e
