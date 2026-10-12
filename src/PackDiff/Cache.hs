-- |
-- Module      : PackDiff.Cache
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Cache of hoogle files in @$XDG_CACHE_HOME/packdiff/hoogle@. Entries are
-- never evicted.

module PackDiff.Cache
    ( CacheKey (..)
    , cacheFileName
    , cacheLookup
    , cacheStore
    ) where

import System.Directory (XdgDirectory(..), doesFileExist, getXdgDirectory)

import qualified Coreutils.Cp as Cp
import qualified Coreutils.Mkdir as Mkdir
import qualified Coreutils.Mv as Mv
import qualified Streamly.FileSystem.Path as Path

import PackDiff.Hash (hashHex)
import PackDiff.WorkDir (currentPid)

data CacheKey = CacheKey
    { ckPackage :: String
      -- ^ package name, or the subdirectory when no package is given
    , ckRevision :: String -- ^ commit SHA or Hackage version
    , ckGhcVersion :: String
    , ckHaddockVersion :: String
    , ckCabalOptions :: [String]
    } deriving (Show)

-- | A readable name with a hash of the cabal options. Characters that are
-- not safe in a file name are replaced.
cacheFileName :: CacheKey -> String
cacheFileName k =
    concatMap safe (ckPackage k) ++ "-" ++ concatMap safe (ckRevision k)
        ++ "-ghc-" ++ ckGhcVersion k
        ++ "-haddock-" ++ ckHaddockVersion k
        ++ "-" ++ hashHex (show (ckCabalOptions k))
        ++ ".txt"

    where

    safe c
        | c `elem` "/\\:*?\"<>| " = "_"
        | otherwise = [c]

cacheDir :: IO FilePath
cacheDir = fmap (++ "/hoogle") (getXdgDirectory XdgCache "packdiff")

cachePath :: CacheKey -> IO FilePath
cachePath k = fmap (\d -> d ++ "/" ++ cacheFileName k) cacheDir

cacheLookup :: CacheKey -> IO (Maybe FilePath)
cacheLookup k = do
    path <- cachePath k
    exists <- doesFileExist path
    pure (if exists then Just path else Nothing)

-- | Copy a hoogle file into the cache and return the path of the entry. The
-- entry is written to a temporary file and renamed into place, so that a
-- concurrent run never reads a partly written entry.
cacheStore :: CacheKey -> FilePath -> IO FilePath
cacheStore k src = do
    dir <- cacheDir
    Mkdir.mkdir (Mkdir.makeParents True) (Path.fromString_ dir)
    path <- cachePath k
    pid <- currentPid
    let tmp = path ++ ".tmp-" ++ show pid
    Cp.cp id (Path.fromString_ src) (Path.fromString_ tmp)
    Mv.mv (Mv.force True) (Path.fromString_ tmp) (Path.fromString_ path)
    pure path
