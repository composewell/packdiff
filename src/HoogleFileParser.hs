module HoogleFileParser where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Data.Char (isUpper)
import Data.List (isPrefixOf)
import Data.Map (Map)
import Streamly.Internal.Data.Fold (Fold)

import qualified Data.Map as Map
import qualified Data.Map.Strict as SMap
import qualified Streamly.Internal.Data.Fold as Fold

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data FixityType
    = FixityD
    | FixityL
    | FixityR
    deriving (Eq, Ord, Show)

type ModuleName = String

data Tagged t v =
    Tagged t v
    deriving (Show)

untag :: Tagged t v -> v
untag (Tagged _ v) = v

data EntityContextType
    = ECFixity (FixityType, Int)
    | ECString String
    deriving (Eq, Show)

showECT :: EntityContextType -> String
showECT (ECFixity x) = show x
showECT (ECString x) = x

data ModuleContext t2 t1 k v =
    ModuleContext
        { mClasses :: Map k (Tagged t1 v)
        , mDataTypes :: Map k (Tagged t2 (Map k (Tagged t1 v)))
        , mFixities :: Map k (Tagged t1 v)
        , mInstances :: Map k (Tagged t2 (Map k (Tagged t1 v)))
        , mNewTypes :: Map k (Tagged t2 (Map k (Tagged t1 v)))
        , mPatterns :: Map k (Tagged t1 v)
        , mTypeAliases :: Map k (Tagged t1 v)
        , mFunctions :: Map k (Tagged t1 v)
        }

type ModuleContextDefault t2 t1 = ModuleContext t2 t1 String EntityContextType

type NameWithContextDefault t = Map String (Tagged t EntityContextType)

defModuleContext :: ModuleContextDefault t2 t1
defModuleContext =
    ModuleContext
        { mClasses = Map.empty
        , mDataTypes = Map.empty
        , mFixities = Map.empty
        , mInstances = Map.empty
        , mNewTypes = Map.empty
        , mPatterns = Map.empty
        , mTypeAliases = Map.empty
        , mFunctions = Map.empty
        }

-- | API of a package.
type API t3 t2 t1 = Map ModuleName (Tagged t3 (ModuleContextDefault t2 t1))

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

-- XXX This could have been much cleaner if there was backtracking in folds
data ParsingModuleState t3 t2 t1
    = PNone (API t3 t2 t1)
    | InModuleContext (API t3 t2 t1) ModuleName (ModuleContextDefault t2 t1)
    | InDataTypeContext
          (API t3 t2 t1)
          ModuleName
          (ModuleContextDefault t2 t1)
          String
          (NameWithContextDefault t1)
    | InNewTypeContext
          (API t3 t2 t1)
          ModuleName
          (ModuleContextDefault t2 t1)
          String
          (NameWithContextDefault t1)

takeWord :: String -> String
takeWord = takeWhile (/= ' ') . dropWhile (== ' ')

dropWord :: String -> String
dropWord = dropWhile (/= ' ') . dropWhile (== ' ')

stripBegin :: String -> String
stripBegin = dropWhile (== ' ')

haddockParseFold :: Monad m => t3 -> t2 -> t1 -> Fold m String (API t3 t2 t1)
haddockParseFold tag3 tag2 tag1 = extract <$> Fold.foldl' step initial

    where

    mapperStr = Tagged tag1 . ECString
    mapperFix = Tagged tag1 . ECFixity

    mCtxAp api name ctx = Map.insert name (Tagged tag3 ctx) api

    dtCtxAp ctx dname dctx =
        ctx {mDataTypes = Map.insert dname (Tagged tag2 dctx) (mDataTypes ctx)}

    tCtxAp ctx dname dctx =
        ctx {mNewTypes = Map.insert dname (Tagged tag2 dctx) (mNewTypes ctx)}

    extract (PNone api) = api
    extract (InModuleContext api name ctx) = mCtxAp api name ctx
    extract (InDataTypeContext api mname mctx dname dctx) =
        mCtxAp api mname (dtCtxAp mctx dname dctx)
    extract (InNewTypeContext api mname mctx dname dctx) =
        mCtxAp api mname (tCtxAp mctx dname dctx)

    initial = PNone Map.empty
    getAfterTypeCtx = reverse . takeWhile (/= '>') . reverse
    getW1AfterTypeCtx = takeWord . getAfterTypeCtx
    getRestAfterTypeCtx = stripBegin . dropWord . getAfterTypeCtx
    getClassName = getW1AfterTypeCtx
    getInstanceOf = getW1AfterTypeCtx
    getInstanceFor = getRestAfterTypeCtx
    getRestAfterW2 = stripBegin . dropWord . dropWord
    getW2 = takeWord . dropWord

    stepOtherScope constr applier api name ctx dname dctx line
        | isUpper (head line) || (head line == '[') =
            let newDCtx = Map.insert (takeWord line) (mapperStr line) dctx
             in constr api name ctx dname newDCtx
        | otherwise =
            let newCtx = applier ctx dname dctx
             in stepModuleScope api name newCtx line

    stepModuleScope api name ctx line
        | "module" `isPrefixOf` line =
            InModuleContext (mCtxAp api name ctx) (getW2 line) defModuleContext
        | "class" `isPrefixOf` line =
            InModuleContext api name
                $ ctx
                      { mClasses =
                            Map.insert
                                (getClassName line)
                                (mapperStr line)
                                (mClasses ctx)
                      }
        | "instance" `isPrefixOf` line =
            let singMap =
                    Tagged tag2
                        $ Map.singleton (getInstanceOf line) (mapperStr line)
                insertF a b = Tagged tag2 (SMap.union (untag a) (untag b))
             in InModuleContext api name
                    $ ctx
                          { mInstances =
                                SMap.insertWith
                                    insertF
                                    (getInstanceFor line)
                                    singMap
                                    (mInstances ctx)
                          }
        | "data" `isPrefixOf` line =
            InDataTypeContext api name ctx (getW2 line) Map.empty
        | "type" `isPrefixOf` line =
            InModuleContext api name
                $ ctx
                      { mTypeAliases =
                            Map.insert
                                (getW2 line)
                                (mapperStr line)
                                (mTypeAliases ctx)
                      }
        | "newtype" `isPrefixOf` line =
            InNewTypeContext api name ctx (getW2 line) Map.empty
        | "infixl" `isPrefixOf` line =
            InModuleContext api name
                $ ctx
                      { mFixities =
                            Map.insert
                                (getRestAfterW2 line)
                                (mapperFix (FixityL, read (getW2 line)))
                                (mFixities ctx)
                      }
        | "infixr" `isPrefixOf` line =
            InModuleContext api name
                $ ctx
                      { mFixities =
                            Map.insert
                                (getRestAfterW2 line)
                                (mapperFix (FixityR, read (getW2 line)))
                                (mFixities ctx)
                      }
        | "infix" `isPrefixOf` line =
            InModuleContext api name
                $ ctx
                      { mFixities =
                            Map.insert
                                (getRestAfterW2 line)
                                (mapperFix (FixityD, read (getW2 line)))
                                (mFixities ctx)
                      }
        | "pattern" `isPrefixOf` line =
            InModuleContext api name
                $ ctx
                      { mPatterns =
                            Map.insert
                                (getW2 line)
                                (mapperStr line)
                                (mPatterns ctx)
                      }
        | otherwise =
            InModuleContext api name
                $ ctx
                      { mFunctions =
                            Map.insert
                                (takeWord line)
                                (mapperStr line)
                                (mFunctions ctx)
                      }

    -- Ignore comments
    step st line
        | "--" `isPrefixOf` line = st
    -- Ignore empty lines
    step st line
        | null line = st
    -- Ignore new lines
    step st line
        | line == "\n" = st
    -- Shift to a new module context from nothing
    step (PNone api) line
        | "module" `isPrefixOf` line =
            InModuleContext api (drop 7 line) defModuleContext
        | otherwise = PNone api
    -- In scope of a module
    step (InModuleContext api name ctx) line = stepModuleScope api name ctx line
    -- In scope of a data type
    step (InDataTypeContext api mname mctx dname dctx) line =
        stepOtherScope InDataTypeContext dtCtxAp api mname mctx dname dctx line
    -- In scope of a new type
    step (InNewTypeContext api mname mctx dname dctx) line =
        stepOtherScope InNewTypeContext tCtxAp api mname mctx dname dctx line
