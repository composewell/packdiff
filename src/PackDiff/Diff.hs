-- |
-- Module      : PackDiff.Diff
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Difference between two APIs. Definitions are compared textually.

module PackDiff.Diff
    ( Change (..)
    , ChildDiff (..)
    , DeclDiff (..)
    , declDiffDecl
    , declHeadChanged
    , ModuleDiff (..)
    , moduleDiffModule
    , APIDiff
    , diffAPI
    ) where

import Control.Applicative ((<|>))
import Data.Map (Map)
import Data.Maybe (fromMaybe, isNothing, mapMaybe)

import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Merge

import PackDiff.API

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | The constructor order is the order of the sections in the output grouped
-- by change.
data Change
    = Removed
    | Changed
    | Deprecated
    | Undeprecated
    | Added
    deriving (Eq, Ord, Show, Enum, Bounded)

data ChildDiff = ChildDiff
    { cdChange :: Change
    , cdOld :: Maybe Child
    , cdNew :: Maybe Child
    } deriving (Eq, Show)

-- | A 'Changed' declaration has changed in its own definition, in its
-- children, or both.
data DeclDiff = DeclDiff
    { ddChange :: Change
    , ddOld :: Maybe Decl
    , ddNew :: Maybe Decl
    , ddChildren :: [ChildDiff]
    } deriving (Eq, Show)

-- | The new declaration, or the old one if it was removed.
declDiffDecl :: DeclDiff -> Decl
declDiffDecl dd =
    fromMaybe (error "declDiffDecl: empty") (ddNew dd <|> ddOld dd)

-- | Whether the declaration line itself (not its children) changed.
declHeadChanged :: DeclDiff -> Bool
declHeadChanged dd =
    case (ddOld dd, ddNew dd) of
        (Just o, Just n) -> declText o /= declText n
        _ -> False

data ModuleDiff = ModuleDiff
    { mdChange :: Change
    , mdOld :: Maybe Module
    , mdNew :: Maybe Module
    , mdDecls :: [DeclDiff]
      -- ^ Changed declarations, sorted by kind and name.
    } deriving (Eq, Show)

-- | The new module, or the old one if it was removed.
moduleDiffModule :: ModuleDiff -> Module
moduleDiffModule md =
    fromMaybe (error "moduleDiffModule: empty") (mdNew md <|> mdOld md)

-- | Changed modules only.
type APIDiff = Map ModuleName ModuleDiff

--------------------------------------------------------------------------------
-- Diffing
--------------------------------------------------------------------------------

deprecationChange :: Bool -> Bool -> Maybe Change
deprecationChange False True = Just Deprecated
deprecationChange True False = Just Undeprecated
deprecationChange _ _ = Nothing

diffChild :: Child -> Child -> Maybe ChildDiff
diffChild o n
    | childText o /= childText n = Just (ChildDiff Changed (Just o) (Just n))
    | otherwise =
        fmap
            (\c -> ChildDiff c (Just o) (Just n))
            (deprecationChange (childDeprecated o) (childDeprecated n))

-- | Children present in the new declaration are in their new order, followed
-- by the removed ones in their old order.
diffChildren :: [Child] -> [Child] -> [ChildDiff]
diffChildren old new = mapMaybe inNew new ++ removed

    where

    byName = Map.fromList . map (\c -> (childName c, c))
    oldMap = byName old
    newMap = byName new

    inNew n =
        case Map.lookup (childName n) oldMap of
            Nothing -> Just (ChildDiff Added Nothing (Just n))
            Just o -> diffChild o n

    removed =
        [ ChildDiff Removed (Just o) Nothing
        | o <- old
        , isNothing (Map.lookup (childName o) newMap)
        ]

diffDecl :: Decl -> Decl -> Maybe DeclDiff
diffDecl o n
    | declText o /= declText n || not (null children) =
        Just (DeclDiff Changed (Just o) (Just n) children)
    | otherwise =
        fmap
            (\c -> DeclDiff c (Just o) (Just n) [])
            (deprecationChange (declDeprecated o) (declDeprecated n))

    where

    children = diffChildren (declChildren o) (declChildren n)

diffDecls :: Map DeclKey Decl -> Map DeclKey Decl -> [DeclDiff]
diffDecls old new =
    Map.elems
        $ Merge.merge
            (Merge.mapMissing (\_ o -> DeclDiff Removed (Just o) Nothing []))
            (Merge.mapMissing (\_ n -> DeclDiff Added Nothing (Just n) []))
            (Merge.zipWithMaybeMatched (\_ o n -> diffDecl o n))
            old
            new

diffModule :: Module -> Module -> Maybe ModuleDiff
diffModule o n =
    case deprecationChange (moduleDeprecated o) (moduleDeprecated n) of
        Just c -> Just (mk c)
        Nothing
            | null decls -> Nothing
            | otherwise -> Just (mk Changed)

    where

    decls = diffDecls (moduleDecls o) (moduleDecls n)
    mk c = ModuleDiff c (Just o) (Just n) decls

-- | @diffAPI old new@.
diffAPI :: API -> API -> APIDiff
diffAPI =
    Merge.merge
        (Merge.mapMissing (\_ o -> ModuleDiff Removed (Just o) Nothing []))
        (Merge.mapMissing (\_ n -> ModuleDiff Added Nothing (Just n) []))
        (Merge.zipWithMaybeMatched (\_ o n -> diffModule o n))
