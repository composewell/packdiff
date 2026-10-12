-- |
-- Module      : PackDiff.Hash
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- A stable hash for names on disk. The output must not change between
-- versions, therefore @hashable@ is not used.

module PackDiff.Hash
    ( fnv1a64
    , hashHex
    ) where

import Data.Bits (xor)
import Data.Functor.Identity (runIdentity)
import Data.Word (Word64)
import Numeric (showHex)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Unicode.Stream as Unicode

-- | 64-bit FNV-1a of the UTF-8 encoding of a string.
fnv1a64 :: String -> Word64
fnv1a64 =
    runIdentity
        . Stream.fold (Fold.foldl' step 0xcbf29ce484222325)
        . Unicode.encodeUtf8
        . Stream.fromList

    where

    step h b = (h `xor` fromIntegral b) * 0x100000001b3

-- | 'fnv1a64' as 16 hex digits.
hashHex :: String -> String
hashHex s =
    let h = showHex (fnv1a64 s) ""
     in replicate (16 - length h) '0' ++ h
