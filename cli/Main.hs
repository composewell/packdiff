module Main
    ( main
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.Monad (when)
import Data.Function ((&))
import Data.List (isInfixOf)
import Streamly.Data.Stream (Stream)
import System.Environment (getArgs)

-- import Debug.Trace (trace)

import qualified Data.Map as Map
import qualified Streamly.Data.Fold as Fold
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

main :: IO ()
main = do
    args <- getArgs
    when (length args < 3) $ fail "Need a target and 2 revisions to compare."
    let target = args !! 0
        rev1 = args !! 1
        rev2 = args !! 2
    (Just file1) <- checkoutAndGenerateHoogleFile target rev1
    (Just file2) <- checkoutAndGenerateHoogleFile target rev2
    putStrLn $ unwords ["File for", rev1, ":", file1]
    putStrLn $ unwords ["File for", rev2, ":", file2]
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
    step "Printing diff"
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

    let diff =
            let filt k v =
                    not (isInternal k)
                        && not (isDeprecatedInBoth v || isDeprecatedInLeft v)
            in Map.filterWithKey filt (diffAPI api1 api2)

    putStrLn $ prettyAPI elems diff

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
