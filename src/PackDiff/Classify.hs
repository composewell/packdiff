-- |
-- Module      : PackDiff.Classify
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Classification of changes as breaking or non-breaking, and the PVP version
-- bump required by a diff.

module PackDiff.Classify
    ( -- * Breaking changes
      isBreakingChild
    , isBreakingDecl
    , moduleBreakingCount
    , breakingCount

      -- * Versions
    , Version
    , parseVersion
    , showVersion
    , compareVersion
    , Bump (..)
    , requiredBump
    , bumpVersion
    ) where

import Data.Char (isDigit)

import PackDiff.Diff

--------------------------------------------------------------------------------
-- Breaking changes
--------------------------------------------------------------------------------

isStructural :: Change -> Bool
isStructural c = c `elem` [Added, Removed, Changed]

-- | An added, removed or changed constructor, field or method changes the
-- definition of its data type or class.
isBreakingChild :: ChildDiff -> Bool
isBreakingChild = isStructural . cdChange

-- | Removals and changes of definitions are breaking. A declaration whose only
-- change is the deprecation of some of its children is not.
--
-- An added orphan instance is breaking under the PVP, but the hoogle file
-- does not mark orphan instances, so added instances are non-breaking.
isBreakingDecl :: DeclDiff -> Bool
isBreakingDecl dd =
    case ddChange dd of
        Removed -> True
        Changed -> declHeadChanged dd || any isBreakingChild (ddChildren dd)
        _ -> False

-- | A removed module counts as one breaking change.
moduleBreakingCount :: ModuleDiff -> Int
moduleBreakingCount md =
    case mdChange md of
        Removed -> 1
        _ -> length (filter isBreakingDecl (mdDecls md))

breakingCount :: [ModuleDiff] -> Int
breakingCount = sum . map moduleBreakingCount

--------------------------------------------------------------------------------
-- Versions
--------------------------------------------------------------------------------

type Version = [Int]

parseVersion :: String -> Maybe Version
parseVersion s
    | null s = Nothing
    | otherwise = mapM readComponent (splitDots s)

    where

    splitDots str =
        case break (== '.') str of
            (a, []) -> [a]
            (a, _ : rest) -> a : splitDots rest
    readComponent c
        | not (null c) && all isDigit c = Just (read c)
        | otherwise = Nothing

showVersion :: Version -> String
showVersion = foldr1 (\a b -> a ++ "." ++ b) . map show

-- | Compare versions, treating missing components as zero, so that @1.2@ and
-- @1.2.0.0@ are equal.
compareVersion :: Version -> Version -> Ordering
compareVersion a b = compare (pad a) (pad b)

    where

    n = max (length a) (length b)
    pad v = v ++ replicate (n - length v) 0

-- | PVP version bumps, for versions of the form @A.B.C.D@.
data Bump
    = BumpMajor -- ^ @A.B@
    | BumpMinor -- ^ @C@
    | BumpPatch -- ^ @D@
    deriving (Eq, Show)

-- | Any breaking change requires a major bump, any other change a minor bump.
requiredBump :: [ModuleDiff] -> Bump
requiredBump mds
    | breakingCount mds > 0 = BumpMajor
    | not (null mds) = BumpMinor
    | otherwise = BumpPatch

-- | The smallest version with the given bump, e.g. @1.3.0.0@ for a major bump
-- of @1.2.0.0@.
bumpVersion :: Bump -> Version -> Version
bumpVersion bump v =
    case bump of
        BumpMajor -> [a, b + 1, 0, 0]
        BumpMinor -> [a, b, c + 1, 0]
        BumpPatch -> [a, b, c, d + 1]

    where

    (a, b, c, d) =
        case v ++ repeat 0 of
            (x1 : x2 : x3 : x4 : _) -> (x1, x2, x3, x4)
            _ -> error "bumpVersion: unreachable"
