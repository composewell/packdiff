
module Pretty
    ( prettyAPI
    , prettyMC
    , prettyD0
    , prettyD1
    , Element(..)
    , printer
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Data.List (intersperse, nub)
import Data.Map (Map)

import qualified Data.Map.Strict as SMap

import HoogleFileParser
import Diff

--------------------------------------------------------------------------------
-- Printing helpers
--------------------------------------------------------------------------------

prettyTag :: StatusTag s -> String
prettyTag Added = "[A]"
prettyTag Removed = "[R]"
prettyTag Same = "[S]"
prettyTag (Changed _) = "[C]"

isDeprecated :: [Annotation] -> Bool
isDeprecated anns =
    let f x =
            case x of
                Deprecated _ -> True
                _ -> False
     in not $ null $ filter f anns

-- Don't print if deprecated in Left
-- Error out if deprecated in Left and not deprecated in Right
-- Error out if deprecated in Right and Added
prettyD0_ ::
       String
    -> Tagged (Attach (Diff [Annotation]) (StatusTag EntityContextType)) EntityContextType
    -> [String]
prettyD0_ _ (Tagged (Attach (DLeft anns) Removed) b) =
    if isDeprecated anns
    then [""]
    else ["[R] " ++ showECT b]
prettyD0_ _ (Tagged (Attach (DRight anns) Added) b) =
    if isDeprecated anns
    then [""]
    else ["[A] " ++ showECT b]
prettyD0_ k (Tagged (Attach (DBoth annsL annsR) (Changed b1)) b) =
    if isDeprecated annsR && isDeprecated annsL
    then [""]
    else if isDeprecated annsR && not (isDeprecated annsL)
         then ["[D] " ++ showECT b1]
         else if not (isDeprecated annsR) && isDeprecated annsL
              then [""]
              else if b1 == b
                   then [""]
                   else concat
                            [ ["[C] " ++ k]
                            , indenter
                                  4
                                  ["[O] " ++ showECT b, "[N] " ++ showECT b1]
                            ]
prettyD0_ _ (Tagged (Attach (DBoth annsL annsR) Same) b) =
    if isDeprecated annsR && not (isDeprecated annsL)
    then ["[D] " ++ showECT b]
    else [""]

prettyD1_ ::
       String
    -> Tagged (Attach (Diff [Annotation]) (StatusTag ())) (Map String (Tagged (Attach (Diff [Annotation]) (StatusTag EntityContextType)) EntityContextType))
    -> [String]
prettyD1_ k (Tagged (Attach (DLeft anns) Removed) b) =
    if isDeprecated anns
    then [""]
    else ["[R] " ++ k]
prettyD1_ k (Tagged (Attach (DRight anns) Added) b) =
    if isDeprecated anns
    then [""]
    else concat [["[A] " ++ k], indenter 4 (prettyD0 b)]
prettyD1_ k (Tagged (Attach (DBoth annsL annsR) (Changed ())) b) =
    if isDeprecated annsR && isDeprecated annsL
    then [""]
    else if isDeprecated annsR && not (isDeprecated annsL)
         then concat [["[D] " ++ k], indenter 4 (prettyD0 b)]
         else if not (isDeprecated annsR) && isDeprecated annsL
              then [""]
              else concat [["[C] " ++ k], indenter 4 (prettyD0 b)]
prettyD1_ k (Tagged (Attach (DBoth annsL annsR) Same) b) =
    if isDeprecated annsR && not (isDeprecated annsL)
    then ["[D] " ++ k]
    else [""]

prettyAPI_ ::
       [Element]
    -> ModuleName
    -> Tagged (Attach (Diff [Annotation]) (StatusTag ())) (ModuleContextDefault (Attach (Diff [Annotation]) (StatusTag ())) (Attach (Diff [Annotation]) (StatusTag EntityContextType)))
    -> [String]
prettyAPI_ _ k (Tagged (Attach (DLeft anns) Removed) b) =
    if isDeprecated anns
    then [""]
    else ["[R] " ++ k]
prettyAPI_ elems k (Tagged (Attach (DRight anns) Added) b) =
    if isDeprecated anns
    then [""]
    else concat [["[A] " ++ k], indenter 4 (prettyMC elems b)]
prettyAPI_ elems k (Tagged (Attach (DBoth annsL annsR) (Changed ())) b) =
    if isDeprecated annsR && isDeprecated annsL
    then [""]
    else if isDeprecated annsR && not (isDeprecated annsL)
         then concat [["[D] " ++ k], indenter 4 (prettyMC elems b)]
         else if not (isDeprecated annsR) && isDeprecated annsL
              then [""]
              else concat [["[C] " ++ k], indenter 4 (prettyMC elems b)]
prettyAPI_ _ k (Tagged (Attach (DBoth annsL annsR) Same) b) =
    if isDeprecated annsR && not (isDeprecated annsL)
    then ["[D] " ++ k]
    else [""]

indenter :: Int -> [String] -> [String]
indenter i = map (replicate i ' ' ++)

printer :: [String] -> String
printer = concat . intersperse "\n" . filter (not . null . stripBegin)

data Element
    = ELClasses
    | ELDataTypes Bool
    | ELFixities
    | ELInstances Bool
    | ELNewTypes Bool
    | ELPatterns
    | ELTypeAliases
    | ELFunctions
    deriving (Eq)

prettyD0 ::
       Map String (Tagged (Attach (Diff [Annotation]) (StatusTag EntityContextType)) EntityContextType)
    -> [String]
prettyD0 = SMap.foldlWithKey step initial

    where

    initial = []

    step a k t = prettyD0_ k t ++ a

prettyD1 ::
       Bool
    -> Map String (Tagged (Attach (Diff [Annotation]) (StatusTag ())) (Map String (Tagged (Attach (Diff [Annotation]) (StatusTag EntityContextType)) EntityContextType)))
    -> [String]
prettyD1 l = SMap.foldlWithKey step initial

    where

    initial = []

    step a k t = prettyD1_ k t ++ a

prettyMC ::
       [Element]
    -> ModuleContextDefault (Attach (Diff [Annotation]) (StatusTag ())) (Attach (Diff [Annotation]) (StatusTag EntityContextType))
    -> [String]
prettyMC elems ctx = concat $ map displayer $ nub elems

    where

    displayer ELClasses = prettyD0 $ mClasses ctx
    displayer (ELDataTypes l) = prettyD1 l $ mDataTypes ctx
    displayer ELFixities = prettyD0 $ mFixities ctx
    displayer (ELInstances l) = prettyD1 l $ mInstances ctx
    displayer (ELNewTypes l) = prettyD1 l $ mNewTypes ctx
    displayer ELPatterns = prettyD0 $ mPatterns ctx
    displayer ELTypeAliases = prettyD0 $ mTypeAliases ctx
    displayer ELFunctions = prettyD0 $ mFunctions ctx


prettyAPI ::
       [Element]
    -> API (Attach (Diff [Annotation]) (StatusTag ())) (Attach (Diff [Annotation]) (StatusTag ())) (Attach (Diff [Annotation]) (StatusTag EntityContextType))
    -> String
prettyAPI elems = printer . SMap.foldlWithKey step initial

    where

    initial = []

    step a k t = prettyAPI_ elems k t ++ a
