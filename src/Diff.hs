module Diff
    ( StatusTag(..)
    , diffAPI
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Data.Map (Map)

import qualified Data.Map.Strict as SMap

import HoogleFileParser

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data StatusTag new
    = Added
    | Removed
    | Changed new
    | Same
    deriving (Eq, Ord)

--------------------------------------------------------------------------------
-- Diffing
--------------------------------------------------------------------------------

diffStatusTaggedMap ::
       (Ord k)
    => (Tagged (StatusTag n) v -> Tagged (StatusTag n) v -> Tagged (StatusTag n) v)
    -> Map k (Tagged (StatusTag n) v)
    -> Map k (Tagged (StatusTag n) v)
    -> Map k (Tagged (StatusTag n) v)
diffStatusTaggedMap unionFunction m1 m2 =
    SMap.unionWith unionFunction m1 m2

unionFunctionDepth0 ::
       Tagged (StatusTag EntityContextType) EntityContextType
    -> Tagged (StatusTag EntityContextType) EntityContextType
    -> Tagged (StatusTag EntityContextType) EntityContextType
unionFunctionDepth0 (Tagged _ v1) (Tagged _ v2) =
    if v1 == v2
    then Tagged Same v1
    else Tagged (Changed v2) v1

unionFunctionDepth1 ::
       Tagged (StatusTag ()) (Map String (Tagged (StatusTag EntityContextType) EntityContextType))
    -> Tagged (StatusTag ()) (Map String (Tagged (StatusTag EntityContextType) EntityContextType))
    -> Tagged (StatusTag ()) (Map String (Tagged (StatusTag EntityContextType) EntityContextType))
unionFunctionDepth1 (Tagged _ v1) (Tagged _ v2) =
    let diffedMap = diffStatusTaggedMap unionFunctionDepth0 v1 v2
     in if hasTagOtherThanSame diffedMap
        then Tagged (Changed ()) diffedMap
        else Tagged Same diffedMap

hasTagOtherThanSame :: Map k (Tagged (StatusTag n) v) -> Bool
hasTagOtherThanSame = SMap.foldl step initial

    where

    initial = False

    step _ (Tagged (Changed _) _) = True
    step _ (Tagged Added _) = True
    step _ (Tagged Removed _) = True
    step b _ = b

unionFunctionDepth2 ::
       Tagged (StatusTag ()) (ModuleContextDefault (StatusTag ()) (StatusTag EntityContextType))
    -> Tagged (StatusTag ()) (ModuleContextDefault (StatusTag ()) (StatusTag EntityContextType))
    -> Tagged (StatusTag ()) (ModuleContextDefault (StatusTag ()) (StatusTag EntityContextType))
unionFunctionDepth2 (Tagged _ m1) (Tagged _ m2) =
    let diffClasses =
            diffStatusTaggedMap unionFunctionDepth0 (mClasses m1) (mClasses m2)
        diffDataTypes =
            diffStatusTaggedMap
                unionFunctionDepth1
                (mDataTypes m1)
                (mDataTypes m2)
        diffFixities =
            diffStatusTaggedMap
                unionFunctionDepth0
                (mFixities m1)
                (mFixities m2)
        diffInstances =
            diffStatusTaggedMap
                unionFunctionDepth1
                (mInstances m1)
                (mInstances m2)
        diffNewTypes =
            diffStatusTaggedMap
                unionFunctionDepth1
                (mNewTypes m1)
                (mNewTypes m2)
        diffPatterns =
            diffStatusTaggedMap
                unionFunctionDepth0
                (mPatterns m1)
                (mPatterns m2)
        diffTypeAliases =
            diffStatusTaggedMap
                unionFunctionDepth0
                (mTypeAliases m1)
                (mTypeAliases m2)
        diffFunctions =
            diffStatusTaggedMap
                unionFunctionDepth0
                (mFunctions m1)
                (mFunctions m2)
        newTag =
            if hasTagOtherThanSame diffFunctions
                   || hasTagOtherThanSame diffTypeAliases
                   || hasTagOtherThanSame diffPatterns
                   || hasTagOtherThanSame diffNewTypes
                   || hasTagOtherThanSame diffInstances
                   || hasTagOtherThanSame diffFixities
                   || hasTagOtherThanSame diffDataTypes
                   || hasTagOtherThanSame diffClasses
            then Changed ()
            else Same
     in Tagged newTag
            $ ModuleContext
                  { mClasses = diffClasses
                  , mDataTypes = diffDataTypes
                  , mFixities = diffFixities
                  , mInstances = diffInstances
                  , mNewTypes = diffNewTypes
                  , mPatterns = diffPatterns
                  , mTypeAliases = diffTypeAliases
                  , mFunctions = diffFunctions
                  }

-- unionFunctionDepth3 if Tagged
diffAPI ::
       API (StatusTag ()) (StatusTag ()) (StatusTag EntityContextType)
    -> API (StatusTag ()) (StatusTag ()) (StatusTag EntityContextType)
    -> API (StatusTag ()) (StatusTag ()) (StatusTag EntityContextType)
diffAPI api1 api2 = diffStatusTaggedMap unionFunctionDepth2 api1 api2
