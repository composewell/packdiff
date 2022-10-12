module Diff
    ( StatusTag(..)
    , diffAPI
    , diffModuleContext
    , Diff(..)
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

data Diff a
    = DLeft a
    | DRight a
    | DBoth a a
    deriving (Show)

-- Left leaning diff
mergeDiff :: Diff a -> Diff a -> Diff a
mergeDiff (DLeft l) (DRight r) = DBoth l r
mergeDiff l _ = l

diffIsEq :: (a -> a -> Bool) -> Diff a -> Diff a -> Bool
diffIsEq eq (DLeft l) (DRight r) = eq l r
diffIsEq _ _ _ = undefined

diffIsEqDoc :: Diff [Annotation] -> Diff [Annotation] -> Bool
diffIsEqDoc l r = diffIsEq (==) l r

--------------------------------------------------------------------------------
-- Diffing
--------------------------------------------------------------------------------

diffStatusTaggedMap ::
       (Ord k)
    => (Tagged t v -> Tagged t v -> Tagged t v)
    -> Map k (Tagged t v)
    -> Map k (Tagged t v)
    -> Map k (Tagged t v)
diffStatusTaggedMap unionFunction m1 m2 =
    SMap.unionWith unionFunction m1 m2

-- Generalize the [Annotation] part by supplying a comparision function
unionFunctionDepth0 ::
       Tagged (Attach (Diff [Annotation]) (StatusTag EntityContextType)) EntityContextType
    -> Tagged (Attach (Diff [Annotation]) (StatusTag EntityContextType)) EntityContextType
    -> Tagged (Attach (Diff [Annotation]) (StatusTag EntityContextType)) EntityContextType
unionFunctionDepth0 (Tagged (Attach l _) v1) (Tagged (Attach r _) v2) =
    if v1 /= v2 || not (diffIsEqDoc l r)
    then Tagged (Attach (mergeDiff l r) (Changed v2)) v1
    else Tagged (Attach (mergeDiff l r) Same) v1

unionFunctionDepth1 ::
       Tagged (Attach (Diff [Annotation]) (StatusTag ())) (Map String (Tagged (Attach (Diff [Annotation]) (StatusTag EntityContextType)) EntityContextType))
    -> Tagged (Attach (Diff [Annotation]) (StatusTag ())) (Map String (Tagged (Attach (Diff [Annotation]) (StatusTag EntityContextType)) EntityContextType))
    -> Tagged (Attach (Diff [Annotation]) (StatusTag ())) (Map String (Tagged (Attach (Diff [Annotation]) (StatusTag EntityContextType)) EntityContextType))
unionFunctionDepth1 (Tagged (Attach l _) v1) (Tagged (Attach r _) v2) =
    let diffedMap = diffStatusTaggedMap unionFunctionDepth0 v1 v2
     in if hasTagOtherThanSame diffedMap || not (diffIsEqDoc l r)
        then Tagged (Attach (mergeDiff l r) (Changed ())) diffedMap
        else Tagged (Attach (mergeDiff l r) Same) diffedMap

hasTagOtherThanSame :: Map k (Tagged (Attach a (StatusTag n)) v) -> Bool
hasTagOtherThanSame = SMap.foldl step initial

    where

    initial = False

    step _ (Tagged (Attach _ (Changed _)) _) = True
    step _ (Tagged (Attach _ Added) _) = True
    step _ (Tagged (Attach _ Removed) _) = True
    step b _ = b

diffModuleContext ::
       ModuleContext (Attach (Diff [Annotation]) (StatusTag ())) (Attach (Diff [Annotation]) (StatusTag EntityContextType)) String EntityContextType
    -> ModuleContext (Attach (Diff [Annotation]) (StatusTag ())) (Attach (Diff [Annotation]) (StatusTag EntityContextType)) String EntityContextType
    -> ModuleContext (Attach (Diff [Annotation]) (StatusTag ())) (Attach (Diff [Annotation]) (StatusTag EntityContextType)) String EntityContextType
diffModuleContext m1 m2 =
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
     in ModuleContext
            { mClasses = diffClasses
            , mDataTypes = diffDataTypes
            , mFixities = diffFixities
            , mInstances = diffInstances
            , mNewTypes = diffNewTypes
            , mPatterns = diffPatterns
            , mTypeAliases = diffTypeAliases
            , mFunctions = diffFunctions
            }

unionFunctionDepth2 ::
       Tagged (Attach (Diff [Annotation]) (StatusTag ())) (ModuleContext (Attach (Diff [Annotation]) (StatusTag ())) (Attach (Diff [Annotation]) (StatusTag EntityContextType)) String EntityContextType)
    -> Tagged (Attach (Diff [Annotation]) (StatusTag ())) (ModuleContext (Attach (Diff [Annotation]) (StatusTag ())) (Attach (Diff [Annotation]) (StatusTag EntityContextType)) String EntityContextType)
    -> Tagged (Attach (Diff [Annotation]) (StatusTag ())) (ModuleContext (Attach (Diff [Annotation]) (StatusTag ())) (Attach (Diff [Annotation]) (StatusTag EntityContextType)) String EntityContextType)
unionFunctionDepth2 (Tagged (Attach l _) m1) (Tagged (Attach r _) m2) =
    let diffedMC = diffModuleContext m1 m2
        newTag =
            if hasTagOtherThanSame (mFunctions diffedMC)
                   || hasTagOtherThanSame (mTypeAliases diffedMC)
                   || hasTagOtherThanSame (mPatterns diffedMC)
                   || hasTagOtherThanSame (mNewTypes diffedMC)
                   || hasTagOtherThanSame (mInstances diffedMC)
                   || hasTagOtherThanSame (mFixities diffedMC)
                   || hasTagOtherThanSame (mDataTypes diffedMC)
                   || hasTagOtherThanSame (mClasses diffedMC)
                   || not (diffIsEqDoc l r)
            then Changed ()
            else Same
     in Tagged (Attach (mergeDiff l r) newTag)  -- XXX Diff between the strings
                                                -- for doc here.
            $ diffedMC

-- unionFunctionDepth3 if Tagged
diffAPI ::
       API (Attach (Diff [Annotation]) (StatusTag ())) (Attach (Diff [Annotation]) (StatusTag ())) (Attach (Diff [Annotation]) (StatusTag EntityContextType))
    -> API (Attach (Diff [Annotation]) (StatusTag ())) (Attach (Diff [Annotation]) (StatusTag ())) (Attach (Diff [Annotation]) (StatusTag EntityContextType))
    -> API (Attach (Diff [Annotation]) (StatusTag ())) (Attach (Diff [Annotation]) (StatusTag ())) (Attach (Diff [Annotation]) (StatusTag EntityContextType))
diffAPI api1 api2 = diffStatusTaggedMap unionFunctionDepth2 api1 api2
