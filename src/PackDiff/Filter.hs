-- |
-- Module      : PackDiff.Filter
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Module selection, change type selection and the @--fail-on@ check.

module PackDiff.Filter
    ( -- * Globs
      Glob
    , globMatch

      -- * Modules
    , ModuleFilter (..)
    , defaultInternalGlobs
    , selectModules
    , isInternal
    , partitionInternal

      -- * Change types
    , parseChangeTypes
    , parseFailOn
    , changeTypeNames
    , hasChangeIn
    ) where

import Data.Map (Map)
import Data.Set (Set)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import PackDiff.API (API, ModuleName)
import PackDiff.Diff

--------------------------------------------------------------------------------
-- Globs
--------------------------------------------------------------------------------

-- | A glob in which @*@ matches any sequence of characters, including dots.
type Glob = String

globMatch :: Glob -> String -> Bool
globMatch ('*' : gs) s = any (globMatch gs) (suffixes s)

    where

    suffixes xs = xs : case xs of
        [] -> []
        (_ : rest) -> suffixes rest
globMatch (g : gs) (c : cs) = g == c && globMatch gs cs
globMatch [] [] = True
globMatch _ _ = False

--------------------------------------------------------------------------------
-- Modules
--------------------------------------------------------------------------------

data ModuleFilter = ModuleFilter
    { mfInclude :: [Glob] -- ^ empty means all modules
    , mfExclude :: [Glob]
    , mfInternal :: [Glob]
    }

defaultInternalGlobs :: [Glob]
defaultInternalGlobs = ["*.Internal.*", "*.Internal"]

matchesAny :: [Glob] -> ModuleName -> Bool
matchesAny globs m = any (`globMatch` m) globs

-- | Keep the modules selected by @--module@ and @--ignore-module@.
selectModules :: ModuleFilter -> API -> API
selectModules mf = Map.filterWithKey (\k _ -> selected k)

    where

    selected m =
        (null (mfInclude mf) || matchesAny (mfInclude mf) m)
            && not (matchesAny (mfExclude mf) m)

isInternal :: ModuleFilter -> ModuleName -> Bool
isInternal mf = matchesAny (mfInternal mf)

-- | (released, internal)
partitionInternal :: ModuleFilter -> Map ModuleName a -> (Map ModuleName a, Map ModuleName a)
partitionInternal mf = Map.partitionWithKey (\k _ -> not (isInternal mf k))

--------------------------------------------------------------------------------
-- Change types
--------------------------------------------------------------------------------

changeTypeNames :: [(String, Change)]
changeTypeNames =
    [ ("added", Added)
    , ("removed", Removed)
    , ("changed", Changed)
    , ("deprecated", Deprecated)
    , ("undeprecated", Undeprecated)
    ]

splitCommas :: String -> [String]
splitCommas s =
    case break (== ',') s of
        (a, []) -> [a]
        (a, _ : rest) -> a : splitCommas rest

-- | Parse a comma separated list of change types, given extra names.
parseWith :: [(String, [Change])] -> String -> Either String (Set Change)
parseWith extra s = fmap (Set.fromList . concat) (mapM one (splitCommas s))

    where

    names = [(n, [c]) | (n, c) <- changeTypeNames] ++ extra
    one name =
        case lookup name names of
            Just cs -> Right cs
            Nothing ->
                Left $ "unknown change type " ++ show name ++ ", expected one of: "
                    ++ unwordsComma (map fst names)
    unwordsComma = foldr1 (\a b -> a ++ ", " ++ b)

-- | Values of @--show@ and @--hide@.
parseChangeTypes :: String -> Either String (Set Change)
parseChangeTypes = parseWith []

-- | Values of @--fail-on@: change types, @breaking@ and @any@.
parseFailOn :: String -> Either String (Set Change)
parseFailOn =
    parseWith
        [ ("breaking", [Removed, Changed])
        , ("any", [minBound .. maxBound])
        ]

-- | Whether a module, declaration or child change of these types is present.
hasChangeIn :: Set Change -> [ModuleDiff] -> Bool
hasChangeIn cs = any inModule

    where

    has c = c `Set.member` cs
    inModule md = has (mdChange md) || any inDecl (mdDecls md)
    inDecl dd = has (ddChange dd) || any (has . cdChange) (ddChildren dd)

