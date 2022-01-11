module Main (main) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.Monad (when)
import Data.Function ((&))
import Streamly.Internal.Data.Stream.IsStream (SerialT)
import System.Environment (getArgs)

import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import qualified Streamly.Internal.FileSystem.File as File
import qualified Streamly.Internal.Unicode.Stream as Unicode
import qualified Streamly.Internal.System.Command as Command

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
     in Command.toLines Fold.toList cmd & Stream.last

checkoutAndGenerateHoogleFile  :: String -> String -> IO (Maybe String)
checkoutAndGenerateHoogleFile target rev = do
    step $ unwords ["Generating haddock file for", rev]
    checkoutRevision rev
    generateHoogleFile target rev

fileToLines :: String -> SerialT IO String
fileToLines path =
    File.toChunks path & Unicode.decodeUtf8Arrays
        & Stream.splitOn (== '\n') Fold.toList

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
    api2 <- fileToLines file2 & Stream.fold (haddockParseFold Added Added Added)
    step "Printing diff"
    putStrLn $ prettyAPI False (diffAPI api1 api2)
