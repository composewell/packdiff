{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
-- |
-- Module      : PackDiff.Obtain
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Resolve refs and obtain their hoogle files.

module PackDiff.Obtain
    ( -- * Requirements
      requiredPrograms
    , checkPrograms

      -- * Environment
    , Env
    , EnvConfig (..)
    , newEnv
    , resolveRepo
    , checkInPlace
    , restoreInPlace

      -- * Obtaining
    , Obtained (..)
    , pinRef
    , obtain
    ) where

import Control.Exception (Handler(..), IOException, catches, throwIO, try)
import Control.Monad (unless, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import Data.List (nub)
import Data.Map (Map)
import Data.Maybe (isJust)
import System.Directory (doesDirectoryExist, doesFileExist, makeAbsolute)
import System.IO (hPutStrLn, stderr)

import qualified Coreutils.Which as Which
import qualified Data.Map.Strict as Map
import qualified Streamly.FileSystem.Path as Path

import PackDiff.API (Hoogle)
import PackDiff.Cabal
import PackDiff.Cache
import PackDiff.Error
import PackDiff.Git (Sha)
import PackDiff.Hash (hashHex)
import PackDiff.HoogleFileParser (readHoogleFile)
import PackDiff.Ref
import PackDiff.WorkDir

import qualified PackDiff.Git as Git

--------------------------------------------------------------------------------
-- Requirements
--------------------------------------------------------------------------------

-- | The programs needed by each ref, as (program, ref) pairs. The ref is the
-- string given on the command line.
requiredPrograms :: BuildConfig -> Bool -> [(String, Ref)] -> [(String, String)]
requiredPrograms bc inPlace = nub . concatMap needs

    where

    build = ["cabal", compilerProgram bc, haddockProgram bc]
    needs (s, ref) =
        map (, s) $
            case ref of
                RefGit _ -> "git" : build
                RefCwd -> [ "git" | inPlace ] ++ build
                RefHackage _ -> build
                RefFile _ -> []

-- | Whether a program exists, given as a name in PATH or as a path.
programExists :: String -> IO Bool
programExists p
    | any (`elem` "/\\") p = doesFileExist p
    | otherwise = fmap isJust (Which.which p)

-- | Exit with a usage error if a program needed by a ref is missing.
checkPrograms :: [(String, String)] -> IO ()
checkPrograms = mapM_ check

    where

    check (prog, ref) = do
        ok <- programExists prog
        unless ok $
            usageError $
                "program " ++ show prog ++ " not found; it is needed for ref "
                    ++ show ref

--------------------------------------------------------------------------------
-- Environment
--------------------------------------------------------------------------------

data EnvConfig = EnvConfig
    { ecBuild :: BuildConfig
    , ecPackage :: Maybe String
    , ecNoCache :: Bool
    , ecInPlace :: Bool
    }

data Env = Env
    { envConfig :: EnvConfig
    , envWorkSpace :: WorkSpace
    , envCwd :: FilePath
    , envCurrentRepo :: IORef (Maybe (FilePath, FilePath))
      -- ^ top level directory and prefix of the current directory
    , envClones :: IORef (Map String FilePath)
      -- ^ work clone of each repository
    , envTools :: IORef (Maybe (String, String))
      -- ^ ghc and haddock versions
    , envOriginalBranch :: IORef (Maybe (FilePath, String))
      -- ^ in-place mode: repository and branch to restore
    }

newEnv :: EnvConfig -> WorkSpace -> IO Env
newEnv cfg ws = do
    cwd <- makeAbsolute "."
    Env cfg ws cwd
        <$> newIORef Nothing
        <*> newIORef Map.empty
        <*> newIORef Nothing
        <*> newIORef Nothing

-- | Memoize an action in an IORef.
cached :: IORef (Maybe a) -> IO a -> IO a
cached ref action = do
    m <- readIORef ref
    case m of
        Just a -> pure a
        Nothing -> do
            a <- action
            writeIORef ref (Just a)
            pure a

currentRepo :: Env -> IO (FilePath, FilePath)
currentRepo env =
    cached (envCurrentRepo env) $ do
        top <- Git.topLevel (envCwd env)
        pre <- Git.prefix (envCwd env)
        pure (top, pre)

toolVersions :: Env -> IO (String, String)
toolVersions env =
    cached (envTools env) $ do
        let bc = ecBuild (envConfig env)
        (,) <$> ghcVersion bc <*> haddockVersion bc

-- | Resolve the value of a repository flag.
resolveRepo :: String -> IO Repo
resolveRepo s = do
    isDir <- doesDirectoryExist s
    case classifyRepo isDir s of
        Left err -> usageError err
        Right (Left dir) -> RepoLocal <$> (makeAbsolute dir >>= Git.topLevel)
        Right (Right url) -> pure (RepoRemote url)

--------------------------------------------------------------------------------
-- In-place mode
--------------------------------------------------------------------------------

-- | Exit with a usage error if tracked files have uncommitted changes.
checkInPlace :: Env -> IO ()
checkInPlace env = do
    (top, _) <- currentRepo env
    dirty <- Git.hasTrackedChanges top
    when dirty $
        usageError "--in-place: tracked files have uncommitted changes"

-- | Check out a revision in the current working tree. The current branch is
-- recorded and printed before the first checkout.
inPlaceCheckout :: Env -> FilePath -> Sha -> IO ()
inPlaceCheckout env top sha = do
    orig <- readIORef (envOriginalBranch env)
    case orig of
        Just _ -> pure ()
        Nothing -> do
            branch <- Git.currentBranch top
            hPutStrLn stderr $ "packdiff: current branch: " ++ branch
            writeIORef (envOriginalBranch env) (Just (top, branch))
    Git.checkout top sha

-- | Check out the original branch again. The checkout is not forced; if it
-- fails, the branch is printed and an 'ObtainError' is thrown.
restoreInPlace :: Env -> IO ()
restoreInPlace env = do
    orig <- readIORef (envOriginalBranch env)
    case orig of
        Nothing -> pure ()
        Just (top, branch) -> do
            r <- try (Git.checkout top branch)
            case r of
                Right () -> writeIORef (envOriginalBranch env) Nothing
                Left (e :: PackDiffError) ->
                    throwIO $ ObtainError $
                        "could not check out the original branch " ++ branch
                            ++ ": " ++ show e

--------------------------------------------------------------------------------
-- Obtaining
--------------------------------------------------------------------------------

data Obtained = Obtained
    { obFile :: FilePath
    , obHoogle :: Hoogle
    }

readObtained :: FilePath -> IO Obtained
readObtained path = Obtained path <$> readHoogleFile (Path.fromString_ path)

-- | Look the hoogle file up in the cache, or build it and store it.
withCache :: Env -> String -> String -> IO FilePath -> IO Obtained
withCache env what rev build
    | ecNoCache (envConfig env) = build >>= readObtained
    | otherwise = do
        (ghcV, haddockV) <- toolVersions env
        let key =
                CacheKey
                    { ckPackage = what
                    , ckRevision = rev
                    , ckGhcVersion = ghcV
                    , ckHaddockVersion = haddockV
                    , ckCabalOptions = bcCabalOptions (ecBuild (envConfig env))
                    }
        hit <- cacheLookup key
        case hit of
            Just path -> do
                hPutStrLn stderr $ "packdiff: using cached " ++ path
                readObtained path
            Nothing -> build >>= cacheStore key >>= readObtained

repoIdentity :: Env -> Repo -> IO String
repoIdentity env repo =
    case repo of
        RepoCurrent -> fst <$> currentRepo env
        RepoLocal top -> pure top
        RepoRemote url -> pure url

-- | The work clone of a repository, created on first use.
workClone :: Env -> Repo -> IO FilePath
workClone env repo = do
    ident <- repoIdentity env repo
    clones <- readIORef (envClones env)
    case Map.lookup ident clones of
        Just dir -> pure dir
        Nothing -> do
            dir <- newWorkDir (envWorkSpace env) (hashHex ident)
            let src = dir ++ "/src"
            case repo of
                RepoRemote _ -> Git.initRepo dir src
                _ -> Git.cloneShared dir ident src
            modifyIORef' (envClones env) (Map.insert ident dir)
            pure dir

resolveSha :: Env -> Repo -> String -> IO Sha
resolveSha env repo rev =
    case repo of
        RepoCurrent -> do
            (top, _) <- currentRepo env
            Git.resolveLocal top rev
        RepoLocal top -> Git.resolveLocal top rev
        RepoRemote url -> Git.resolveRemote (envCwd env) url rev

-- | The directory to run cabal in, relative to the root of a ref: the root
-- with @--package@, otherwise the subdirectory corresponding to the current
-- directory for refs of the current repository.
subdirFor :: Env -> Repo -> IO FilePath
subdirFor env repo =
    case (ecPackage (envConfig env), repo) of
        (Nothing, RepoCurrent) -> snd <$> currentRepo env
        _ -> pure ""

-- | Append a relative directory, which may be empty, to a directory.
(</>) :: FilePath -> FilePath -> FilePath
dir </> "" = dir
dir </> sub = dir ++ "/" ++ sub

-- | The cache key part naming what was built.
cacheWhat :: Env -> FilePath -> String
cacheWhat env sub =
    case ecPackage (envConfig env) of
        Just pkg -> pkg
        Nothing
            | null sub -> "subdir-root"
            | otherwise -> "subdir-" ++ sub

obtainGit :: Env -> Repo -> String -> IO Obtained
obtainGit env repo rev = do
    sha <- resolveSha env repo rev
    sub <- subdirFor env repo
    withCache env (cacheWhat env sub) sha (build sha sub)

    where

    cfg = envConfig env
    bc = ecBuild cfg
    pkg = ecPackage cfg
    build sha sub
        | ecInPlace cfg && repo == RepoCurrent = do
            (top, _) <- currentRepo env
            inPlaceCheckout env top sha
            buildHoogle bc (top </> sub) pkg Nothing
        | otherwise = do
            wc <- workClone env repo
            let src = wc ++ "/src"
            case repo of
                RepoRemote url -> Git.fetchSha src url sha
                _ -> pure ()
            Git.checkout src sha
            buildHoogle bc (src </> sub) pkg (Just (wc ++ "/build"))

-- | The directory to build the working directory in: the current directory,
-- or with @--package@ the top level directory of the current repository. The
-- root of a project that is not in a git repository is the current directory.
projectRoot :: Env -> IO FilePath
projectRoot env =
    case ecPackage (envConfig env) of
        Nothing -> pure (envCwd env)
        Just _ ->
            (fst <$> currentRepo env)
                `catches`
                    [ Handler (\(_ :: PackDiffError) -> pure (envCwd env))
                    , Handler (\(_ :: IOException) -> pure (envCwd env))
                    ]

-- | The working directory, built in place. Not cached.
obtainCwd :: Env -> IO Obtained
obtainCwd env = do
    dir <- projectRoot env
    let cfg = envConfig env
    buildHoogle (ecBuild cfg) dir (ecPackage cfg) Nothing >>= readObtained

obtainHackage :: Env -> String -> HackageVersion -> IO Obtained
obtainHackage env pkg hv = do
    ver <-
        resolveHackageVersion pkg $
            case hv of
                HackageLatest -> Nothing
                HackageVersion v -> Just v
    withCache env pkg ("hackage-" ++ ver) (build ver)

    where

    build ver = do
        dir <- newWorkDir (envWorkSpace env) (pkg ++ "-" ++ ver)
        src <- cabalGet dir pkg ver
        buildHoogle
            (ecBuild (envConfig env)) src (Just pkg) (Just (dir ++ "/build"))

-- | Resolve a ref of the current repository to a commit SHA, and @.@ to the
-- SHA of @HEAD@ in in-place mode. In-place mode moves @HEAD@, so all refs must
-- be pinned before the first checkout.
pinRef :: Env -> Repo -> Ref -> IO Ref
pinRef env repo ref =
    case (ref, repo) of
        (RefGit rev, RepoCurrent) -> RefGit <$> resolveSha env RepoCurrent rev
        (RefCwd, _)
            | ecInPlace (envConfig env) ->
                RefGit <$> resolveSha env RepoCurrent "HEAD"
        _ -> pure ref

-- | Obtain the hoogle file of a ref. The repository is used for git refs, the
-- package name for Hackage refs.
obtain :: Env -> Repo -> Maybe String -> Ref -> IO Obtained
obtain env repo hackagePkg ref =
    case ref of
        RefFile path -> readObtained path
        RefCwd
            | ecInPlace (envConfig env) -> obtainGit env RepoCurrent "HEAD"
            | otherwise -> obtainCwd env
        RefGit rev -> obtainGit env repo rev
        RefHackage hv ->
            case hackagePkg of
                Just pkg -> obtainHackage env pkg hv
                Nothing ->
                    usageError "a Hackage ref needs the package name (--package)"
