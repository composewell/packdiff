-- |
-- Module      : Main
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC

module Main
    ( main
    ) where

import qualified PackDiff.Main

main :: IO ()
main = PackDiff.Main.main
