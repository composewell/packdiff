
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
       Show d
    => Map String (Tagged (Attach d (StatusTag EntityContextType)) EntityContextType)
    -> [String]
prettyD0 = SMap.foldlWithKey step initial

    where

    initial = []

    step a k (Tagged (Attach d Added) b) =
        unwords ["[A]", show d, k, ":", showECT b] : a
    step a k (Tagged (Attach d Removed) b) =
        unwords ["[R]", show d, k, ":", showECT b] : a
    -- step a k (Tagged Same b) = unwords ["[S]", k, ":", showECT b] : a
    step a _ (Tagged (Attach _ Same) _) = a
    step a k (Tagged (Attach d (Changed b1)) b) =
        concat
            [ [unwords ["[C]", show d, k, ":"]]
            , indenter
                  4
                  [unwords ["[OLD]", showECT b], unwords ["[NEW]", showECT b1]]
            , a
            ]

prettyD1 ::
       Show d
    => Bool
    -> Map String (Tagged (Attach d (StatusTag ())) (Map String (Tagged (Attach d (StatusTag EntityContextType)) EntityContextType)))
    -> [String]
prettyD1 l = SMap.foldlWithKey step initial

    where

    initial = []

    step a _ (Tagged (Attach _d Same) _) = a
    step a k (Tagged (Attach d t) b) =
        if l
        then concat
                 [ [unwords [prettyTag t, show d, k]]
                 , indenter 4 (prettyD0 b)
                 , a
                 ]
        else concat [[unwords [prettyTag t, show d, k]], a]

prettyMC ::
       Show d
    => [Element]
    -> ModuleContextDefault (Attach d (StatusTag ())) (Attach d (StatusTag EntityContextType))
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
       Show d
    => [Element]
    -> API (Attach d (StatusTag ())) (Attach d (StatusTag ())) (Attach d (StatusTag EntityContextType))
    -> String
prettyAPI elems = printer . SMap.foldlWithKey step initial

    where

    initial = []

    step a _ (Tagged (Attach _ Same) _) = a
    step a k (Tagged (Attach d t) b) =
        let ptfy =
                concat
                    [ [unwords [prettyTag t, show d, k]]
                    , indenter 4 (prettyMC elems b)
                    ]
         in concat [ptfy, a]
