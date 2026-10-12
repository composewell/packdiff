{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : PackDiff.Main
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- The packdiff program.

module PackDiff.Main
    ( main
    , defaultRefs
    ) where

import Control.Exception
    ( AsyncException(..)
    , Handler(..)
    , SomeException(..)
    , catches
    , displayException
    , finally
    , throwIO
    , uninterruptibleMask_
    )
import Data.Function ((&))
import Data.Maybe (isNothing)
import System.Console.ANSI (hSupportsANSI)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, hSetEncoding, stderr, stdout, utf8)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Streamly.Console.Stdio as Stdio
import qualified Streamly.FileSystem.FileIO as File
import qualified Streamly.FileSystem.Path as Path

import PackDiff.API
import PackDiff.Cabal (BuildConfig(..))
import PackDiff.Classify
import PackDiff.Diff
import PackDiff.Error
import PackDiff.Filter
import PackDiff.Obtain
import PackDiff.Options
import PackDiff.Pretty
import PackDiff.Ref
import PackDiff.Signals
import PackDiff.WorkDir

--------------------------------------------------------------------------------
-- Refs and repositories
--------------------------------------------------------------------------------

-- | Fill in the default refs: @HEAD .@ for diff and check, @.@ for api.
defaultRefs :: Command -> [String] -> Either String [String]
defaultRefs cmd refs =
    case (cmd, refs) of
        (CmdApi, []) -> Right ["."]
        (CmdApi, [r]) -> Right [r]
        (CmdApi, _) -> Left "api takes at most one ref"
        (_, []) -> Right ["HEAD", "."]
        (_, [r1]) -> Right [r1, "."]
        (_, [r1, r2]) -> Right [r1, r2]
        _ -> Left "diff and check take at most two refs"

-- | A ref as given on the command line, parsed, with the repository flag
-- that applies to it.
data RefArg = RefArg
    { raString :: String
    , raRef :: Ref
    , raRepoFlag :: Maybe String
    }

-- | Parse the refs and assign the repository flags to them.
refArgs :: Options -> IO [RefArg]
refArgs opts = do
    strs <- either usageError pure (defaultRefs (optCommand opts) (optRefs opts))
    refs <- mapM (either usageError pure . parseRef) strs
    let flags =
            case optCommand opts of
                CmdApi -> [optRepo opts]
                _ -> [ orElse (optOldRepo opts) (optRepo opts)
                     , orElse (optNewRepo opts) (optRepo opts)
                     ]
        args = zipWith3 RefArg strs refs flags
    checkRepoFlags opts args
    pure args

    where

    orElse (Just a) _ = Just a
    orElse Nothing b = b

-- | A repository flag that applies only to refs that are not git refs is a
-- usage error.
checkRepoFlags :: Options -> [RefArg] -> IO ()
checkRepoFlags opts args = do
    case (optOldRepo opts, args) of
        (Just _, a : _) | not (isGitRef (raRef a)) ->
            usageError "--old-repo applies only to a git ref, REF1 is not one"
        _ -> pure ()
    case (optNewRepo opts, args) of
        (Just _, [_, a]) | not (isGitRef (raRef a)) ->
            usageError "--new-repo applies only to a git ref, REF2 is not one"
        _ -> pure ()
    case optRepo opts of
        Just _ | not (any (isGitRef . raRef) args) ->
            usageError "--repo applies only to git refs, none is given"
        _ -> pure ()

repoOf :: RefArg -> IO Repo
repoOf a = maybe (pure RepoCurrent) resolveRepo (raRepoFlag a)

--------------------------------------------------------------------------------
-- Obtaining APIs
--------------------------------------------------------------------------------

-- | The package name in a hoogle file.
packageNameOf :: RefArg -> Obtained -> IO String
packageNameOf a ob =
    case hooglePackages (obHoogle ob) of
        [p] -> pure p
        _ ->
            usageError $
                "cannot take the package name from ref " ++ raString a
                    ++ "; use --package"

obtainArg :: Env -> Maybe String -> RefArg -> IO Obtained
obtainArg env pkg a = do
    repo <- repoOf a
    obtain env repo pkg (raRef a)

-- | Obtain two refs. Without --package, a Hackage ref takes the package name
-- from the other ref, which is therefore obtained first.
obtainPair :: Env -> Options -> RefArg -> RefArg -> IO (Obtained, Obtained)
obtainPair env opts a1 a2 =
    case optPackage opts of
        Just p -> (,) <$> obtainArg env (Just p) a1 <*> obtainArg env (Just p) a2
        Nothing
            | isHackageRef (raRef a1) && isHackageRef (raRef a2) ->
                usageError "--package is required when both refs are Hackage refs"
            | isHackageRef (raRef a1) -> do
                o2 <- obtainArg env Nothing a2
                p <- packageNameOf a2 o2
                o1 <- obtainArg env (Just p) a1
                pure (o1, o2)
            | isHackageRef (raRef a2) -> do
                o1 <- obtainArg env Nothing a1
                p <- packageNameOf a1 o1
                o2 <- obtainArg env (Just p) a2
                pure (o1, o2)
            | otherwise ->
                (,) <$> obtainArg env Nothing a1 <*> obtainArg env Nothing a2

--------------------------------------------------------------------------------
-- Output
--------------------------------------------------------------------------------

useColor :: ColorMode -> IO Bool
useColor mode =
    case mode of
        ColorAlways -> pure True
        ColorNever -> pure False
        ColorAuto -> do
            noColor <- lookupEnv "NO_COLOR"
            ansi <- hSupportsANSI stdout
            pure (maybe True null noColor && ansi)

moduleFilter :: Options -> ModuleFilter
moduleFilter opts =
    ModuleFilter
        { mfInclude = optModules opts
        , mfExclude = optIgnoreModules opts
        , mfInternal =
            case optInternalModules opts of
                [] -> defaultInternalGlobs
                globs -> globs
        }

-- | Which change types are printed.
visibility :: Options -> IO (Change -> Bool)
visibility opts =
    case (shown, optHide opts) of
        (Just _, Just _) -> usageError "--show and --hide cannot be used together"
        (Just s, Nothing) -> pure (`Set.member` s)
        (Nothing, Just h) -> pure (`Set.notMember` h)
        (Nothing, Nothing) -> pure (const True)

    where

    breaking = Set.fromList [Removed, Changed]
    shown =
        case (optShow opts, optBreakingOnly opts) of
            (s, False) -> s
            (s, True) -> Just (maybe breaking (Set.union breaking) s)

selectedAPI :: Options -> Obtained -> API
selectedAPI opts = selectModules (moduleFilter opts) . hoogleAPI . obHoogle

--------------------------------------------------------------------------------
-- Commands
--------------------------------------------------------------------------------

-- | Released and internal module diffs.
diffPair :: Options -> Obtained -> Obtained -> (APIDiff, APIDiff)
diffPair opts o1 o2 =
    partitionInternal (moduleFilter opts)
        $ diffAPI (selectedAPI opts o1) (selectedAPI opts o2)

diffCommand :: Env -> Options -> RefArg -> RefArg -> IO ExitCode
diffCommand env opts a1 a2 = do
    visible <- visibility opts
    color <- useColor (optColor opts)
    hPutStrLn stderr $
        "packdiff: comparing " ++ raString a1 ++ " with " ++ raString a2
    (o1, o2) <- obtainPair env opts a1 a2
    let (released, internal) = diffPair opts o1 o2
        ro = RenderOptions
                { roGroupBy = optGroupBy opts
                , roQuiet = optQuiet opts
                , roModulesOnly = optModulesOnly opts
                , roVisible = visible
                }
    putStr $ renderLines color (renderDiff ro released internal)
    pure $ case optFailOn opts of
        Just cs | hasChangeIn cs (Map.elems released) -> ExitFailure 1
        _ -> ExitSuccess

-- | The version of a ref, from its single @\@version@ line.
versionOf :: RefArg -> Obtained -> IO Version
versionOf a ob =
    case hoogleVersions (obHoogle ob) of
        [v] | Just ver <- parseVersion v -> pure ver
        [v] -> usageError $ "invalid version " ++ show v ++ " in ref " ++ raString a
        vs ->
            usageError $
                "ref " ++ raString a ++ " has " ++ show (length vs)
                    ++ " @version lines, check needs exactly one"

checkCommand :: Env -> Options -> RefArg -> RefArg -> IO ExitCode
checkCommand env opts a1 a2 = do
    hPutStrLn stderr $
        "packdiff: checking the version of " ++ raString a2
            ++ " against " ++ raString a1
    (o1, o2) <- obtainPair env opts a1 a2
    base <- versionOf a1 o1
    declared <- versionOf a2 o2
    let (released, _) = diffPair opts o1 o2
        mds = Map.elems released
        bump = requiredBump mds
        required = bumpVersion bump base
        sufficient = compareVersion declared required /= LT
        label a v = raString a ++ " (" ++ showVersion v ++ ")"
        (changes, bumpName) =
            case bump of
                BumpMajor -> ("breaking changes", "major")
                BumpMinor
                    | hasChangeIn (Set.singleton Added) mds ->
                        ("new symbols added", "minor")
                    | otherwise -> ("non-breaking changes", "minor")
                BumpPatch -> ("no API changes", "patch")
    putStrLn $
        changes ++ " between " ++ label a1 base ++ " and "
            ++ label a2 declared ++ ": bump " ++ bumpName ++ " ("
            ++ showVersion required ++ ")"
    putStrLn $
        "declared version " ++ showVersion declared ++ " of " ++ raString a2
            ++ " is " ++ if sufficient then "sufficient" else "insufficient"
    pure (if sufficient then ExitSuccess else ExitFailure 1)

apiCommand :: Env -> Options -> RefArg -> IO ExitCode
apiCommand env opts a = do
    color <- useColor (optColor opts)
    ob <- obtainArg env (optPackage opts) a
    case optFormat opts of
        FormatHoogle ->
            File.readChunks (Path.fromString_ (obFile ob)) & Stdio.putChunks
        FormatText -> do
            let (released, internal) =
                    partitionInternal (moduleFilter opts) (selectedAPI opts ob)
            putStr $ renderLines color (renderAPI released internal)
    pure ExitSuccess

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

run :: Options -> IO ExitCode
run opts = do
    args <- refArgs opts
    _ <- visibility opts -- report conflicting flags before any work
    let bc = BuildConfig (optCompiler opts) (optCabalOptions opts)
    checkPrograms
        (requiredPrograms bc (optInPlace opts)
            [(raString a, raRef a) | a <- args])
    root <- maybe defaultWorkRoot pure (optWorkDir opts)
    let cfg = EnvConfig
            { ecBuild = bc
            , ecPackage = optPackage opts
            , ecNoCache = optNoCache opts
            , ecInPlace = optInPlace opts
            }
    withWorkSpace root $ \ws -> do
        env <- newEnv cfg ws
        let inPlaceRefs =
                [ a | a <- args
                , raRef a == RefCwd
                    || isGitRef (raRef a) && isNothing (raRepoFlag a)
                ]
        if optInPlace opts && not (null inPlaceRefs)
        then do
            checkInPlace env
            pinned <- mapM (pin env) args
            command env pinned
                `finally` uninterruptibleMask_ (restoreInPlace env)
        else command env args

    where

    pin env a = do
        repo <- repoOf a
        ref <- pinRef env repo (raRef a)
        pure a { raRef = ref }

    command env args =
        case (optCommand opts, args) of
            (CmdDiff, [a1, a2]) -> diffCommand env opts a1 a2
            (CmdCheck, [a1, a2]) -> checkCommand env opts a1 a2
            (CmdApi, [a]) -> apiCommand env opts a
            _ -> usageError "wrong number of refs"

-- | All exceptions are reported and mapped to exit code 2 or 3.
main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    opts <- parseOptions
    code <-
        (installTerminationHandlers >> run opts)
            `catches`
                [ Handler (\(e :: ExitCode) -> throwIO e)
                , Handler (\(e :: PackDiffError) ->
                    failWith (errorExitCode e) (displayException e))
                , Handler (\(Terminated s) -> failWith 3 ("terminated by " ++ s))
                , Handler (\(e :: AsyncException) ->
                    case e of
                        UserInterrupt -> failWith 3 "interrupted"
                        _ -> failWith 3 (displayException e))
                -- The inner exception is displayed without the backtrace that
                -- displayException of SomeException adds.
                , Handler (\(SomeException e) -> failWith 3 (displayException e))
                ]
    exitWith code

    where

    failWith code msg = do
        hPutStrLn stderr ("packdiff: " ++ msg)
        pure (ExitFailure code)

