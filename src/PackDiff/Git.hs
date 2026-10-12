-- |
-- Module      : PackDiff.Git
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- The git commands used by packdiff.

module PackDiff.Git
    ( Sha
      -- * Queries
    , topLevel
    , prefix
    , resolveLocal
    , resolveRemote
    , matchRemoteRef
    , hasTrackedChanges
    , currentBranch

      -- * Work clones
    , cloneShared
    , initRepo
    , fetchSha
    , checkout
    ) where

import Control.Monad (void)
import Data.Char (isHexDigit, isSpace)
import Data.Maybe (listToMaybe)

import PackDiff.Error (onObtainError, usageError)
import PackDiff.Process

type Sha = String

git :: FilePath -> [String] -> Cmd
git dir = Cmd (Just dir) "git"

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

firstLine :: String -> String
firstLine = trim . takeWhile (/= '\n')

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

-- | The top level directory of the repository containing a directory.
topLevel :: FilePath -> IO FilePath
topLevel dir =
    fmap firstLine (readCmd (git dir ["rev-parse", "--show-toplevel"]))
        `onObtainError` usageError ("not in a git repository: " ++ dir)

-- | The path of a directory relative to the top level directory of its
-- repository, with a trailing slash, or empty at the top level.
prefix :: FilePath -> IO FilePath
prefix dir = fmap firstLine (readCmd (git dir ["rev-parse", "--show-prefix"]))

-- | Resolve a revision to a commit SHA in a local repository.
resolveLocal :: FilePath -> String -> IO Sha
resolveLocal repo rev =
    fmap firstLine
        (readCmd (git repo ["rev-parse", "--verify", "--quiet", rev ++ "^{commit}"]))
        `onObtainError`
            usageError ("unknown git revision " ++ show rev ++ " in " ++ repo)

isFullSha :: String -> Bool
isFullSha s = length s == 40 && all isHexDigit s

-- | Find a revision in the output of @git ls-remote@: a branch, a tag (the
-- commit of an annotated tag), @HEAD@, a full ref name, or a full SHA.
matchRemoteRef :: [String] -> String -> Maybe Sha
matchRemoteRef out rev
    | isFullSha rev = Just rev
    | otherwise =
        listToMaybe
            [ sha
            | name <- candidates
            , (sha, ref) <- refs
            , ref == name
            ]

    where

    refs = [(sha, ref) | l <- out, (sha, '\t' : ref) <- [break (== '\t') l]]
    candidates =
        [ rev
        , "refs/tags/" ++ rev ++ "^{}"
        , "refs/tags/" ++ rev
        , "refs/heads/" ++ rev
        ]

-- | Resolve a branch, tag or full SHA of a remote repository with
-- @git ls-remote@.
resolveRemote :: FilePath -> String -> String -> IO Sha
resolveRemote cwd url rev
    | isFullSha rev = pure rev
    | otherwise = do
        out <- readCmdLines (git cwd ["ls-remote", url])
        case matchRemoteRef out rev of
            Just sha -> pure sha
            Nothing ->
                usageError $
                    "revision " ++ show rev ++ " not found in " ++ url
                        ++ "; a revision of a remote repository must be a "
                        ++ "branch, a tag or a full commit SHA"

-- | Whether tracked files have uncommitted changes.
hasTrackedChanges :: FilePath -> IO Bool
hasTrackedChanges repo =
    fmap (not . null . trim)
        (readCmd (git repo ["status", "--porcelain", "--untracked-files=no"]))

-- | The current branch, or the SHA if @HEAD@ is detached.
currentBranch :: FilePath -> IO String
currentBranch repo = do
    branch <-
        fmap firstLine (readCmd (git repo ["symbolic-ref", "--short", "-q", "HEAD"]))
            `onObtainError` pure ""
    if null branch
    then fmap firstLine (readCmd (git repo ["rev-parse", "HEAD"]))
    else pure branch

--------------------------------------------------------------------------------
-- Work clones
--------------------------------------------------------------------------------

-- | @git clone --shared --no-checkout@: the clone reads objects from the
-- source repository through @.git/objects/info/alternates@.
cloneShared :: FilePath -> FilePath -> FilePath -> IO ()
cloneShared cwd repo dest =
    void $ runCmd (git cwd ["clone", "-q", "--shared", "--no-checkout", repo, dest])

initRepo :: FilePath -> FilePath -> IO ()
initRepo cwd dest = void $ runCmd (git cwd ["init", "-q", dest])

-- | Fetch only the files of one commit.
fetchSha :: FilePath -> String -> Sha -> IO ()
fetchSha repo url sha =
    void $ runCmd (git repo ["fetch", "-q", "--depth", "1", url, sha])

checkout :: FilePath -> String -> IO ()
checkout repo rev = void $ runCmd (git repo ["checkout", "-q", rev])
