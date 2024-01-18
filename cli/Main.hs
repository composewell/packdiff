module Main
    ( main
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.Monad (when)
import Data.Function ((&))
import Data.List (isInfixOf, isPrefixOf)
import Streamly.Data.Stream (Stream)
import System.Environment (getArgs)
import Data.Char (ord)
import Streamly.Data.Fold (Fold)

-- import Debug.Trace (trace)

import qualified Data.Map as Map
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Parser as Parser
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.FileSystem.File as File
import qualified Streamly.Internal.System.Command as Command
import qualified Streamly.Internal.Unicode.Stream as Unicode

import Diff
import HoogleFileParser
import Pretty

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

step :: String -> IO ()
step msg = do
    putStrLn $ unlines
        [ ""
        , "---------------------------------"
        , msg
        , "---------------------------------"
        ]

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

checkoutRevision :: String -> IO ()
checkoutRevision rev =
    let cmd = unwords ["git checkout", rev]
     in Command.toStdout cmd

generateHoogleFile :: String -> String -> IO (Maybe String)
generateHoogleFile target bd =
    let cmd =
            unwords
                [ "cabal haddock"
                , target
                , "--haddock-hoogle"
                , "--builddir=" ++ bd
                ]
     in Command.toLines Fold.toList cmd & Stream.fold Fold.latest

-- XXX Cache the file for ther version by default.
checkoutAndGenerateHoogleFile  :: String -> String -> IO (Maybe String)
checkoutAndGenerateHoogleFile target rev = do
    step $ unwords ["Generating haddock file for", rev]
    checkoutRevision rev
    generateHoogleFile target ("dist-newstyle-" ++ rev)

fileToLines :: String -> Stream IO String
fileToLines path =
    File.readChunks path & Unicode.decodeUtf8Chunks
        & Stream.foldMany (Fold.takeEndBy_ (== '\n') Fold.toList)

isDeprecated :: [Annotation] -> Bool
isDeprecated anns =
    let f x =
            case x of
                Deprecated _ -> True
                _ -> False
     in not $ null $ filter f anns

isInternal :: ModuleName -> Bool
isInternal x = "Internal" `isInfixOf` x

mainSingle :: [String] -> IO ()
mainSingle args = do
    when (length args < 2) $ fail "Need a target and a revision."
    let target = args !! 0
        rev1 = args !! 1
    (Just file1) <- checkoutAndGenerateHoogleFile target rev1
    putStrLn $ unwords ["File for", rev1, ":", file1]
    api1 <-
        fileToLines file1
            & Stream.fold (haddockParseFold Added Added Added)
            & fmap
                  (mapAPITags
                       (mapAttachment (DRight . parseDoc))
                       (mapAttachment (DRight . parseDoc))
                       (mapAttachment (DRight . parseDoc)))
    let elems =
            [ ELClasses
            , ELDataTypes True
            , ELFixities
            , ELInstances True
            , ELNewTypes True
            , ELPatterns
            , ELTypeAliases
            , ELFunctions
            ]
    let isDeprecatedInRight (Tagged (Attach (DRight anns) _) _) =
            isDeprecated anns
        isDeprecatedInRight (Tagged (Attach (DBoth _ anns) _) _) =
            isDeprecated anns
        isDeprecatedInRight _ = False

    let apiReleased =
            Map.filterWithKey
                   (\k v -> not (isInternal k) && not (isDeprecatedInRight v))
                   api1
    let apiInternal =
            Map.filterWithKey (\k _ -> isInternal k) api1

    step "Released API"
    putStrLn $ prettyAPI elems apiReleased
    step "Internal API"
    putStrLn $ prettyAPI elems apiInternal

mainDiff :: [String] -> IO ()
mainDiff args = do
    when (length args < 4)
        $ fail "target1 cabal-file revision-for-target1 revision-for-target-2"
    let target = args !! 0
        cabalFilePath = args !! 1
        revTarget1 = args !! 2
        revTarget2 = args !! 3
    (Just file1) <- checkoutAndGenerateHoogleFile target revTarget1
    (Just file2) <- checkoutAndGenerateHoogleFile target revTarget2
    putStrLn $ unwords ["File for", target, revTarget1, ":", file1]
    putStrLn $ unwords ["File for", target, revTarget2, ":", file2]
    api1 <-
        fileToLines file1
            & Stream.fold (haddockParseFold Removed Removed Removed)
            & fmap
                  (mapAPITags
                       (mapAttachment (DLeft . parseDoc))
                       (mapAttachment (DLeft . parseDoc))
                       (mapAttachment (DLeft . parseDoc)))
    api2 <-
        fileToLines file2 & Stream.fold (haddockParseFold Added Added Added)
            & fmap
                  (mapAPITags
                       (mapAttachment (DRight . parseDoc))
                       (mapAttachment (DRight . parseDoc))
                       (mapAttachment (DRight . parseDoc)))

    step "API Annotations"
    putStrLn "[A] : Added"
    putStrLn "[R] : Removed"
    putStrLn "[C] : Changed"
    putStrLn "[O] : Old definition"
    putStrLn "[N] : New definition"
    putStrLn "[D] : Deprecated"

    let elems =
            [ ELClasses
            , ELDataTypes True
            , ELFixities
            , ELInstances True
            , ELNewTypes True
            , ELPatterns
            , ELTypeAliases
            , ELFunctions
            ]
    let isDeprecatedInBoth (Tagged (Attach (DBoth annl annr) _) _) =
            isDeprecated annl && isDeprecated annr
        isDeprecatedInBoth _ = False

        isDeprecatedInLeft (Tagged (Attach (DLeft anns) _) _) =
            isDeprecated anns
        isDeprecatedInLeft (Tagged (Attach (DBoth anns _) _) _) =
            isDeprecated anns
        isDeprecatedInLeft _ = False

        hasChanged (Tagged (Attach (DBoth _ _) _) _) = True
        hasChanged _ = False

    let diffRel =
            let filt k v =
                    not (isInternal k)
                        && not (isDeprecatedInBoth v || isDeprecatedInLeft v)
             in Map.filterWithKey filt (diffAPI api1 api2)
    let diffInt =
            let filt k _ = isInternal k
             in Map.filterWithKey filt (diffAPI api1 api2)
    step "API diff"
    putStrLn $ prettyAPI elems diffRel
    step "Internal API diff"
    putStrLn $ prettyAPI elems diffInt

    let diffRelChanged = Map.filter hasChanged diffRel
    step "Changed Released API diff"
    putStrLn $ prettyAPI elems diffRelChanged

    step "Validating PVP"
    if Map.size diffRelChanged > 0
    then do
        ecmv <- getMajorVersionFrom cabalFilePath
        case ecmv of
            Left err -> error err
            Right (a2, b2) -> do
                (a1, b1) <- getLatestMajorVersionFromGitTag $ target ++ "-"
                let cmpRes =
                        case compare a2 a1 of
                            EQ -> compare b2 b1
                            x -> x
                case cmpRes of
                    GT -> do
                        putStrLn "Major version is bumped."
                        putStrLn $ "Prev: " ++ show (a1, b1)
                        putStrLn $ "Curr: " ++ show (a2, b2)
                    _ -> do
                        putStrLn "Need to bump major version"
                        putStrLn $ "Prev: " ++ show (a1, b1)
                        putStrLn $ "Curr: " ++ show (a2, b2)
                        error "PVP"
    else pure ()

{-# INLINE intEndBy_ #-}
intEndBy_ :: Monad m => Fold m Char Int
intEndBy_ = Fold.takeEndBy_ (== '.') (Fold.foldl' stp 0)

    where

    stp a c = a * 10 + fromIntegral (ord c - 48)

fMajorVersionFromString :: Monad m => Fold m Char (Int, Int)
fMajorVersionFromString = (,) <$> intEndBy_ <*> intEndBy_

getMajorVersionFrom :: FilePath -> IO (Either String (Int, Int))
getMajorVersionFrom cabalFile = do
    mval <- getVersionLine
    case mval of
        Nothing -> pure $ Left "getCurrentMajorVersion: empty"
        Just val -> do
            putStrLn $ "getCurrentMajorVersion[Version line]: " ++ show val
            eres <- Stream.parse pMajorVersionLine $ Stream.fromList val
            case eres of
                Left _ -> pure $ Left "getCurrentMajorVersion: parsing failed"
                Right res -> do
                    putStrLn
                        $ "getCurrentMajorVersion[Parsed major]: " ++ show res
                    pure $ Right res

    where

    getVersionLine =
        fileToLines cabalFile
            & Stream.filter ("version:" `isPrefixOf`)
            & Stream.fold Fold.one

    pMajorVersionLine =
        Parser.fromFold (Fold.takeEndBy_ (== ':') Fold.drain)
            *> Parser.dropWhile  (== ' ')
            *> Parser.fromFold fMajorVersionFromString

getLatestMajorVersionFromGitTag :: String -> IO (Int, Int)
getLatestMajorVersionFromGitTag prefix = do
    versions <-
        Command.toLines Fold.toList "git tag"
            & Stream.filter (prefix `isPrefixOf`)
            & fmap (drop (length prefix))
            & Stream.fold Fold.toList
    putStrLn $ "getLatestMajorVersionFromGitTag[Versions]: \n" ++ show versions
    parsedMajors <-
        Stream.fromList versions
            & Stream.mapM
                  (Stream.fold fMajorVersionFromString . Stream.fromList)
            & Stream.toList
    putStrLn
        $ "getLatestMajorVersionFromGitTag[Parsed]: \n" ++ show parsedMajors
    Stream.fromList parsedMajors
        & Stream.fold
              (Fold.foldl'
                  (\(a1, b1) (a2, b2) ->
                       case compare a2 a1 of
                           EQ -> (a2, max b1 b2)
                           GT -> (a2, b2)
                           LT -> (a1, b1)
                  )
                  (0, 0))

main :: IO ()
main = do
    args <- getArgs
    when (length args < 1) $ fail "Need a mode. [single] or [diff]."
    let mode = args !! 0
    case mode of
        "single" -> mainSingle (tail args)
        "diff" -> mainDiff (tail args)
        _ -> error "Improper mode."


-- TODO:

-- Deprecated in previous release but not in this release
-- Deprecated in this release and in previous release
-- Deprecated in this release

-- APIs that don't have since annotations
-- APIs that don't have complexity annotations
-- APIs that have any Pre-release or Internal annotations

-- inspect module, individual elements

-- Breaking changes: API in the released modules that existed before but is
-- changed now.
-- - Get released modules from (streamly:0.8.3)
-- - Get released modules from (streamly:master + streamly-core:master)
-- - Filter out modules that have been deprecated in (streamly:0.8.3)
-- - Filter out APIs that have been deprecated in (streamly:0.8.3)

{-


main :: IO ()
main = do
    (Just file083) <-
         checkoutAndGenerateHoogleFile "streamly" "v0.8.3"
    (Just fileMaster) <-
         checkoutAndGenerateHoogleFile "streamly" "release-tasks"
    (Just fileCore) <-
         checkoutAndGenerateHoogleFile "streamly-core" "release-tasks"
    step "Hoogle files"
    putStrLn file083
    putStrLn fileMaster
    putStrLn fileCore
    api083 <-
        fileToLines file083
            & Stream.fold (haddockParseFold Removed Removed Removed)
            & fmap
                  (mapAPITags
                       (mapAttachment (DLeft . parseDoc))
                       (mapAttachment (DLeft . parseDoc))
                       (mapAttachment (DLeft . parseDoc)))
    apiMaster <-
        fileToLines fileMaster
            & Stream.fold (haddockParseFold Added Added Added)
            & fmap
                  (mapAPITags
                       (mapAttachment (DRight . parseDoc))
                       (mapAttachment (DRight . parseDoc))
                       (mapAttachment (DRight . parseDoc)))
    apiCore <-
        fileToLines fileCore & Stream.fold (haddockParseFold Added Added Added)
            & fmap
                  (mapAPITags
                       (mapAttachment (DRight . parseDoc))
                       (mapAttachment (DRight . parseDoc))
                       (mapAttachment (DRight . parseDoc)))
    let api = mergeNonConflictingAPI apiMaster apiCore
    let isDeprecated anns =
            let f x =
                    case x of
                        Deprecated _ -> True
                        _ -> False
             in not $ null $ filter f anns

        isDeprecatedInBoth (Tagged (Attach (DBoth annl annr) _) _) =
            isDeprecated annl && isDeprecated annr
        isDeprecatedInBoth _ = False

        isDeprecatedInLeft (Tagged (Attach (DLeft anns) _) _) =
            isDeprecated anns
        isDeprecatedInLeft (Tagged (Attach (DBoth anns _) _) _) =
            isDeprecated anns
        isDeprecatedInLeft _ = False

        existsInBoth (Tagged (Attach (DBoth _ _) _) _) = True
        existsInBoth _ = False

        isDeprecatedInRight (Tagged (Attach (DRight anns) _) _) =
            isDeprecated anns
        isDeprecatedInRight (Tagged (Attach (DBoth _ anns) _) _) =
            isDeprecated anns
        isDeprecatedInRight _ = False

        isInternal x = "Internal" `isInfixOf` x

        isNew (Tagged (Attach (DRight _) _) _) = True
        isNew _ = False

    let api083Exposed = Map.filterWithKey (\k v -> not (isInternal k)) api083
    let apiExposed = Map.filterWithKey (\k v -> not (isInternal k)) api

    let diff0 = diffAPI api083Exposed apiExposed

    let diff1 =
            Map.filterWithKey
                (\k v -> not (isDeprecatedInBoth v) && existsInBoth v)
                diff0

    let diff2 = Map.filterWithKey (\k v -> isDeprecatedInRight v) diff1

    let diff3 =
            Map.filterWithKey
                (\k v -> k == "Streamly.Network.Socket")
                diff1

    let elems =
            [ ELClasses
            , ELDataTypes True
            , ELFixities
            , ELInstances True
            , ELNewTypes True
            , ELPatterns
            , ELTypeAliases
            , ELFunctions
            ]

    -- putStrLn $ prettyAPI elems diff1

    putStrLn
        $ printer
        $ prettyMC
              elems
              (diffModuleContext
                   (untag (api083Exposed Map.! "Streamly.Prelude"))
                   (foldl1
                      mergeModulesLeaningLeft $
                        map untag
                        [ apiExposed Map.! "Streamly.Data.Stream"
                        , apiExposed Map.! "Streamly.Data.Stream.Concurrent"
                        , apiExposed Map.! "Streamly.Data.Stream.Exception"
                        , apiExposed Map.! "Streamly.Data.Stream.Time"
                        , apiExposed Map.! "Streamly.Data.Fold"
                        ]))

-}
