-- |
-- Module      : PackDiff.API
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- The API of a package, as described by a hoogle file.

module PackDiff.API
    ( -- * Declarations
      DeclKind (..)
    , Child (..)
    , Decl (..)
    , DeclKey
    , declKey

      -- * Modules
    , ModuleName
    , Module (..)
    , moduleSize

      -- * API
    , API
    , mergeNonConflictingAPI

      -- * Hoogle file
    , Hoogle (..)
    ) where

import Data.Map (Map)

import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------
-- Declarations
--------------------------------------------------------------------------------

-- | The kind of a top level declaration. The constructor order is the order in
-- which the kinds are printed.
data DeclKind
    = KData -- ^ data type or newtype
    | KTypeSyn
    | KClass
    | KInstance
    | KPattern
    | KFunction
    | KFixity
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | A constructor or field of a data type, or a method of a class.
data Child = Child
    { childName :: String
    , childText :: String
    , childDeprecated :: Bool
    } deriving (Eq, Show)

data Decl = Decl
    { declKind :: DeclKind
    , declName :: String
      -- ^ The name of the entity. For an instance it is the instance head.
    , declText :: String
      -- ^ The line in the hoogle file. For a data type, newtype or class it is
      -- the declaration head.
    , declDoc :: [String]
    , declDeprecated :: Bool
    , declChildren :: [Child]
      -- ^ Constructors and fields, or class methods, in hoogle file order.
    } deriving (Eq, Show)

type DeclKey = (DeclKind, String)

declKey :: Decl -> DeclKey
declKey d = (declKind d, declName d)

--------------------------------------------------------------------------------
-- Modules
--------------------------------------------------------------------------------

type ModuleName = String

data Module = Module
    { moduleDoc :: [String]
    , moduleDeprecated :: Bool
    , moduleDecls :: Map DeclKey Decl
    } deriving (Eq, Show)

-- | Number of top level entities in a module.
moduleSize :: Module -> Int
moduleSize = Map.size . moduleDecls

--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------

type API = Map ModuleName Module

-- | Union of two APIs that have no module in common. A module present in both
-- is an error.
mergeNonConflictingAPI :: API -> API -> API
mergeNonConflictingAPI =
    Map.unionWithKey (\k _ _ -> error ("Conflicting module name " ++ k))

--------------------------------------------------------------------------------
-- Hoogle file
--------------------------------------------------------------------------------

-- | A parsed hoogle file. A file made by concatenating the hoogle files of
-- several packages has several @\@package@ and @\@version@ lines.
data Hoogle = Hoogle
    { hooglePackages :: [String]
    , hoogleVersions :: [String]
    , hoogleAPI :: API
    } deriving (Eq, Show)
