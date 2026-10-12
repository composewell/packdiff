-- |
-- Module      : PackDiff.Ref
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Refs given on the command line and the repositories of git refs.

module PackDiff.Ref
    ( -- * Refs
      HackageVersion (..)
    , Ref (..)
    , parseRef
    , isGitRef
    , isHackageRef

      -- * Repositories
    , Repo (..)
    , classifyRepo
    , isRemoteUrl
    ) where

import Data.List (isInfixOf)

--------------------------------------------------------------------------------
-- Refs
--------------------------------------------------------------------------------

data HackageVersion
    = HackageLatest
    | HackageVersion String
    deriving (Eq, Show)

data Ref
    = RefGit String -- ^ a git revision
    | RefCwd -- ^ the current working directory, @.@
    | RefHackage HackageVersion
    | RefFile FilePath -- ^ a hoogle file
    deriving (Eq, Show)

-- | Parse a ref of the form @<scope>:<value>@. A ref without a scope is a git
-- ref, except @.@ and @hackage@.
parseRef :: String -> Either String Ref
parseRef s =
    case s of
        "" -> Left "empty ref"
        "." -> Right RefCwd
        "hackage" -> Right (RefHackage HackageLatest)
        _ ->
            case break (== ':') s of
                ("git", ':' : rev) -> gitRef rev
                ("hackage", ':' : "latest") -> Right (RefHackage HackageLatest)
                ("hackage", ':' : ver)
                    | null ver -> Left "empty version in ref: hackage:"
                    | otherwise -> Right (RefHackage (HackageVersion ver))
                ("file", ':' : path)
                    | null path -> Left "empty path in ref: file:"
                    | otherwise -> Right (RefFile path)
                (scope, ':' : _) -> Left $ "unknown ref scope: " ++ scope
                (rev, _) -> gitRef rev

    where

    gitRef rev
        | null rev = Left "empty git revision"
        | head rev == '-' = Left $ "invalid git revision: " ++ rev
        | otherwise = Right (RefGit rev)

isGitRef :: Ref -> Bool
isGitRef (RefGit _) = True
isGitRef _ = False

isHackageRef :: Ref -> Bool
isHackageRef (RefHackage _) = True
isHackageRef _ = False

--------------------------------------------------------------------------------
-- Repositories
--------------------------------------------------------------------------------

-- | The repository of a git ref.
data Repo
    = RepoCurrent -- ^ the repository of the current directory
    | RepoLocal FilePath -- ^ absolute path of the top level directory
    | RepoRemote String -- ^ URL
    deriving (Eq, Ord, Show)

-- | A URL, or the scp-like syntax @user\@host:path@ understood by git.
isRemoteUrl :: String -> Bool
isRemoteUrl s = "://" `isInfixOf` s || scpLike

    where

    scpLike =
        case break (== ':') s of
            (host, ':' : _) -> '@' `elem` host && '/' `notElem` host
            _ -> False

-- | Classify the value of a repository flag, given whether it names an
-- existing directory. A directory takes precedence over a URL.
classifyRepo :: Bool -> String -> Either String (Either FilePath String)
classifyRepo isDir s
    | isDir = Right (Left s)
    | isRemoteUrl s = Right (Right s)
    | otherwise =
        Left $ "repository is neither a directory nor a URL: " ++ s
