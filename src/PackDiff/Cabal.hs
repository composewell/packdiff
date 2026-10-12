-- |
-- Module      : PackDiff.Cabal
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Building hoogle files with cabal, and Hackage releases.

module PackDiff.Cabal
    ( -- * Build configuration
      BuildConfig (..)
    , compilerProgram
    , haddockProgram
    , ghcVersion
    , haddockVersion

      -- * Hoogle files
    , hoogleFileFromOutput
    , buildHoogle

      -- * Hackage
    , parseVersionList
    , latestVersion
    , resolveHackageVersion
    , cabalGet
    ) where

import Data.Char (isSpace)
import Data.List (isPrefixOf, isSuffixOf, stripPrefix)
import Data.Maybe (fromMaybe, mapMaybe)

import PackDiff.Classify (Version, parseVersion, showVersion)
import PackDiff.Error (obtainError, onObtainError)
import PackDiff.Process

--------------------------------------------------------------------------------
-- Build configuration
--------------------------------------------------------------------------------

data BuildConfig = BuildConfig
    { bcCompiler :: Maybe String -- ^ @-w@
    , bcCabalOptions :: [String]
    }

compilerProgram :: BuildConfig -> String
compilerProgram = fromMaybe "ghc" . bcCompiler

-- | The directory part of a path, if any.
directoryOf :: String -> Maybe String
directoryOf p =
    case break isSep (reverse p) of
        (_, _ : rdir) -> Just (reverse rdir)
        (_, []) -> Nothing

    where

    isSep c = c == '/' || c == '\\'

-- | The haddock program used by cabal for the selected compiler.
--
-- XXX cabal looks for @haddock-<ghc-version>@ and @haddock@ next to the
-- compiler and then in PATH. This uses @haddock@ next to the compiler if the
-- compiler is given as a path, otherwise @haddock@ in PATH. It is used only
-- for the version in the cache key.
haddockProgram :: BuildConfig -> String
haddockProgram bc =
    case bcCompiler bc >>= directoryOf of
        Just dir -> dir ++ "/haddock"
        Nothing -> "haddock"

ghcVersion :: BuildConfig -> IO String
ghcVersion bc =
    fmap trim
        (readCmd (Cmd Nothing (compilerProgram bc) ["--numeric-version"]))

-- | The version in the output of @haddock --version@, e.g. @Haddock version
-- 2.30.0, (c) ...@.
haddockVersion :: BuildConfig -> IO String
haddockVersion bc = do
    out <- readCmd (Cmd Nothing (haddockProgram bc) ["--version"])
        `onObtainError`
            readCmd (Cmd Nothing "haddock" ["--version"])
    pure $ case words out of
        ("Haddock" : "version" : v : _) -> takeWhile (/= ',') v
        _ -> trim (takeWhile (/= '\n') out)

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

--------------------------------------------------------------------------------
-- Hoogle files
--------------------------------------------------------------------------------

-- | The hoogle file in the output of @cabal haddock --haddock-hoogle@: the
-- last path ending in @.txt@ after @Documentation created:@.
--
-- XXX cabal does not provide a machine readable way to find the generated
-- hoogle file, so its human readable output is used.
hoogleFileFromOutput :: String -> Maybe FilePath
hoogleFileFromOutput out =
    case concatMap (filter (".txt" `isSuffixOf`) . candidates) (lines out) of
        [] -> Nothing
        xs -> Just (last xs)

    where

    candidates l =
        let t = trim l
         in case stripPrefix "Documentation created:" t of
                Just rest -> [trim rest]
                Nothing -> [t | not (null t)]

-- | Run @cabal haddock --haddock-hoogle@ in a directory and return the path of
-- the generated hoogle file.
buildHoogle ::
       BuildConfig
    -> FilePath -- ^ directory to run cabal in
    -> Maybe String -- ^ target package
    -> Maybe FilePath -- ^ build directory
    -> IO FilePath
buildHoogle bc dir target builddir = do
    out <- runCmd (Cmd (Just dir) "cabal" args)
    case hoogleFileFromOutput out of
        Just path
            | isAbsolute path -> pure path
            | otherwise -> pure (dir ++ "/" ++ path)
        Nothing ->
            obtainError $
                "could not find the hoogle file in the output of cabal haddock"
                    ++ " in " ++ dir

    where

    args =
        ["haddock"]
            ++ maybe [] pure target
            ++ ["--haddock-hoogle"]
            ++ maybe [] (\d -> ["--builddir=" ++ d]) builddir
            ++ maybe [] (\c -> ["--with-compiler=" ++ c]) (bcCompiler bc)
            ++ bcCabalOptions bc
    isAbsolute p =
        "/" `isPrefixOf` p
            || case p of
                (_ : ':' : _) -> True -- Windows drive
                _ -> "\\" `isPrefixOf` p

--------------------------------------------------------------------------------
-- Hackage
--------------------------------------------------------------------------------

-- | Versions of a package in the output of @cabal list --simple-output@, one
-- @<pkg> <version>@ line per version. Other packages matching the search are
-- skipped.
parseVersionList :: String -> String -> [Version]
parseVersionList pkg out =
    mapMaybe version (lines out)

    where

    version l =
        case words l of
            [p, v] | p == pkg -> parseVersion v
            _ -> Nothing

latestVersion :: [Version] -> Maybe Version
latestVersion [] = Nothing
latestVersion vs = Just (maximum vs)

-- | Resolve a Hackage version from cabal's local package index. 'Nothing'
-- selects the latest version.
resolveHackageVersion :: String -> Maybe String -> IO String
resolveHackageVersion pkg mver = do
    out <- readCmd (Cmd Nothing "cabal" ["list", "--simple-output", pkg])
        `onObtainError` pure ""
    let versions = parseVersionList pkg out
    case mver of
        Nothing ->
            maybe notFound (pure . showVersion) (latestVersion versions)
        Just v
            | Just pv <- parseVersion v, pv `elem` versions -> pure v
            | otherwise -> notFound

    where

    what = maybe pkg (\v -> pkg ++ "-" ++ v) mver
    notFound =
        obtainError $
            what ++ " not found in cabal's package index; "
                ++ "run 'cabal update' if it was released recently"

-- | Unpack a release with @cabal get@ into a directory, and return the
-- directory of the package source.
cabalGet :: FilePath -> String -> String -> IO FilePath
cabalGet dir pkg ver = do
    let pkgver = pkg ++ "-" ++ ver
    _ <- runCmd (Cmd (Just dir) "cabal" ["get", pkgver, "--destdir=" ++ dir])
    pure (dir ++ "/" ++ pkgver)
