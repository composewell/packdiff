module Pretty
    ( prettyAPI
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Data.List (intersperse, isInfixOf)
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

prettyD0 ::
       Map String (Tagged (StatusTag EntityContextType) EntityContextType)
    -> [String]
prettyD0 = SMap.foldlWithKey step initial

    where

    initial = []

    step a k (Tagged Added b) = unwords ["[A]", k, ":", take 50 (showECT b)] : a
    step a k (Tagged Removed b) =
        unwords ["[R]", k, ":", take 50 (showECT b)] : a
    -- step a k (Tagged Same b) = unwords ["[S]", k, ":", take 50 (showECT b)] : a
    step a _ (Tagged Same _) = a
    step a k (Tagged (Changed b1) b) =
        concat
            [ [unwords ["[C]", k, ":"]]
            , indenter
                  4
                  [ unwords ["[OLD]", take 50 (showECT b)]
                  , unwords ["[NEW]", take 50 (showECT b1)]
                  ]
            , a
            ]

prettyD1 ::
       Map String (Tagged (StatusTag ()) (Map String (Tagged (StatusTag EntityContextType) EntityContextType)))
    -> [String]
prettyD1 = SMap.foldlWithKey step initial

    where

    initial = []

    step a _ (Tagged Same _) = a
    step a k (Tagged t b) =
        concat [[unwords [prettyTag t, k]], indenter 4 (prettyD0 b), a]

prettyMC ::
       ModuleContextDefault (StatusTag ()) (StatusTag EntityContextType)
    -> [String]
prettyMC ctx =
    concat
        [ prettyD0 (mClasses ctx) -- "Classes"
        , prettyD1 (mDataTypes ctx) -- "DataTypes"
        , prettyD0 (mFixities ctx) -- "Fixities"
        , prettyD1 (mInstances ctx) -- "Instances"
        , prettyD1 (mNewTypes ctx) -- "NewTypes"
        , prettyD0 (mPatterns ctx) -- "Patterns"
        , prettyD0 (mTypeAliases ctx) -- "TypeAliases"
        , prettyD0 (mFunctions ctx) -- "Functions"
        ]

prettyAPI ::
       Bool
    -> API (StatusTag ()) (StatusTag ()) (StatusTag EntityContextType)
    -> String
prettyAPI ignoreInternal = printer . SMap.foldlWithKey step initial

    where

    initial = []

    step a _ (Tagged Same _) = a
    step a k (Tagged t b) =
        let ptfy = concat [[unwords [prettyTag t, k]], indenter 4 (prettyMC b)]
         in if ignoreInternal
            then if "Internal" `isInfixOf` k
                 then a
                 else concat [ptfy, a]
            else concat [ptfy, a]
