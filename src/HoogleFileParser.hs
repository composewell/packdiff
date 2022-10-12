
module HoogleFileParser
    ( -- Types
      FixityType(..)
    , ModuleName
    , Doc

      -- Attachments
    , Attach(..)
    , mapAttachment

      -- Tags
    , Tagged(..)
    , untag

      -- Lowest level context
    , EntityContextType(..)
    , showECT

      -- Module context
    , ModuleContext(..)
    , ModuleContextDefault
    , NameWithContextDefault
    , defModuleContext
    , mergeModulesLeaningLeft

      -- API
    , API
    , mapAPITags

      -- API combinators
    , mergeNonConflictingAPI

      -- Additional attached information
    , Annotation(..)
    , parseDoc

      -- Hoogle file parser
    , haddockParseFold

      -- Helpers
    , stripBegin
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.Applicative ((<|>))
import Data.Char (isUpper)
import Data.List (isPrefixOf)
import Data.Map (Map)
import Streamly.Data.Fold (Fold)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Map as Map
import qualified Data.Map.Strict as SMap
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Parser as Parser
import qualified Streamly.Data.Stream as Stream

-- import Debug.Trace (trace)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data FixityType
    = FixityD
    | FixityL
    | FixityR
    deriving (Eq, Ord, Show)

type ModuleName = String

type Doc = String

data Attach b a = Attach b a

mapAttachment :: (a -> b) -> Attach a c -> Attach b c
mapAttachment f (Attach a c) = (Attach (f a) c)

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

type ModuleContextDefault t2 t1
     = ModuleContext t2 t1 String EntityContextType

type NameWithContextDefault t
     = Map String (Tagged t EntityContextType)

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

mapAPITags ::
       (t3 -> s3) -> (t2 -> s2) -> (t1 -> s1) -> API t3 t2 t1 -> API s3 s2 s1
mapAPITags f3 f2 f1 api = Map.map (fmapTagged f3 fmapTaggedMC) api

    where

    fmapTagged ft fv (Tagged t val) = Tagged (ft t) (fv val)

    fmapTaggedMC mc =
        mc
            { mClasses = Map.map (fmapTagged f1 id) (mClasses mc)
            , mDataTypes =
                  Map.map
                      (fmapTagged f2 (Map.map (fmapTagged f1 id)))
                      (mDataTypes mc)
            , mFixities = Map.map (fmapTagged f1 id) (mFixities mc)
            , mInstances =
                  Map.map
                      (fmapTagged f2 (Map.map (fmapTagged f1 id)))
                      (mInstances mc)
            , mNewTypes =
                  Map.map
                      (fmapTagged f2 (Map.map (fmapTagged f1 id)))
                      (mNewTypes mc)
            , mPatterns = Map.map (fmapTagged f1 id) (mPatterns mc)
            , mTypeAliases = Map.map (fmapTagged f1 id) (mTypeAliases mc)
            , mFunctions = Map.map (fmapTagged f1 id) (mFunctions mc)
            }

_mapAPITagsPropogate ::
       (t3 -> s3)
    -> (t3 -> t2 -> s2)
    -> (t3 -> t2 -> t1 -> s1)
    -> (t3 -> t1 -> s1)
    -> API t3 t2 t1
    -> API s3 s2 s1
_mapAPITagsPropogate _f3 _f2 _f1 _f1_ _api = undefined
{-
    Map.map (fmapTagged f3 fmapTaggedMC) api

    where

    fmapTagged ft fv (Tagged t val) = Tagged (ft t) (fv t val)

    fmapTagged1 t3 ft fv (Tagged t val) = Tagged (ft t) (fv t val)

    fmapTagged2 t3 t2 ft fv (Tagged t val) = Tagged (ft t) (fv t val)

    fmapTaggedMC t3 mc =
        mc
            { mClasses = Map.map (fmapTagged1 t3 (f1_ t3) (flip const)) (mClasses mc)
            , mDataTypes =
                  Map.map
                      (fmapTagged1 t3 (f2 t3) (\t3 t2 -> Map.map (fmapTagged2 f1 id)))
                      (mDataTypes mc)
            , mFixities = Map.map (fmapTagged f1 id) (mFixities mc)
            , mInstances =
                  Map.map
                      (fmapTagged f2 (Map.map (fmapTagged f1 id)))
                      (mInstances mc)
            , mNewTypes =
                  Map.map
                      (fmapTagged f2 (Map.map (fmapTagged f1 id)))
                      (mNewTypes mc)
            , mPatterns = Map.map (fmapTagged f1 id) (mPatterns mc)
            , mTypeAliases = Map.map (fmapTagged f1 id) (mTypeAliases mc)
            , mFunctions = Map.map (fmapTagged f1 id) (mFunctions mc)
            }
-}

data Annotation
    = Complexity String
    | Since String
    | Deprecated String
    | OtherAnn String
    deriving (Show, Eq)

parseDoc :: String -> [Annotation]
parseDoc = map parseAnnotation . getItalics . clenseToWords

    where

    parseAnnotation wrd
        | "Since" `isPrefixOf` wrd = Since wrd
        | "O" `isPrefixOf` wrd = Complexity wrd
        | "Deprecated" `isPrefixOf` wrd = Deprecated wrd
        | otherwise = OtherAnn wrd

    getItalics =
        let parseItalicAfterItalic =
                Parser.manyTill (Parser.one) (Parser.listEq "</i>") Fold.toList
            parserBeforeItalic =
                Parser.manyTill (Parser.one) (Parser.listEq "<i>") Fold.drain
            mapStr x
                | null x = Nothing
                | otherwise = Just x
            parser =
                (mapStr <$> (parserBeforeItalic *> parseItalicAfterItalic))
                    <|> (const Nothing <$> (Parser.fromFold Fold.drain))
            fromParserResult = either (error . show) id
         in unsafePerformIO
                . Stream.fold Fold.toList
                . Stream.catMaybes
                . fmap fromParserResult
                . Stream.parseMany parser . Stream.fromList
    clenseToWords =
        unwords . map removeIntroHyphens . filter (not . null) . lines

    removeIntroHyphens ('-':'-':' ':'|':rest) = rest
    removeIntroHyphens ('-':'-':rest) = rest
    removeIntroHyphens line = error $ "Incorrectly parsed: " ++ line

mergeNonConflictingAPI :: API t3 t2 t1 -> API t3 t2 t1 -> API t3 t2 t1
mergeNonConflictingAPI =
    SMap.unionWithKey (\k -> error ("Conflicting module name " ++ k))

mergeModulesLeaningLeft ::
       ModuleContextDefault t2 t1
    -> ModuleContextDefault t2 t1
    -> ModuleContextDefault t2 t1
mergeModulesLeaningLeft mcl mcr =
    ModuleContext
        { mClasses = Map.union (mClasses mcl) (mClasses mcr)
        , mDataTypes = Map.union (mDataTypes mcl) (mDataTypes mcr)
        , mFixities = Map.union (mFixities mcl) (mFixities mcr)
        , mInstances = Map.union (mInstances mcl) (mInstances mcr)
        , mNewTypes = Map.union (mNewTypes mcl) (mNewTypes mcr)
        , mPatterns = Map.union (mPatterns mcl) (mPatterns mcr)
        , mTypeAliases = Map.union (mTypeAliases mcl) (mTypeAliases mcr)
        , mFunctions = Map.union (mFunctions mcl) (mFunctions mcr)
        }

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

-- XXX This could have been much cleaner if there was backtracking in folds
data ParsingModuleState t3 t2 t1
    = PNone (API (Attach Doc t3) (Attach Doc t2) (Attach Doc t1))
    | InModuleContext
          String -- Doc level 2
          (API (Attach Doc t3) (Attach Doc t2) (Attach Doc t1))
          ModuleName
          (ModuleContextDefault (Attach Doc t2) (Attach Doc t1))
    | InDataTypeContext
          String -- Doc level 2
          String -- Doc level 1
          (API (Attach Doc t3) (Attach Doc t2) (Attach Doc t1))
          ModuleName
          (ModuleContextDefault (Attach Doc t2) (Attach Doc t1))
          String
          (NameWithContextDefault (Attach Doc t1))
    | InNewTypeContext
          String -- Doc level 2
          String -- Doc level 1
          (API (Attach Doc t3) (Attach Doc t2) (Attach Doc t1))
          ModuleName
          (ModuleContextDefault (Attach Doc t2) (Attach Doc t1))
          String
          (NameWithContextDefault (Attach Doc t1))

takeWord :: String -> String
takeWord = takeWhile (/= ' ') . dropWhile (== ' ')

dropWord :: String -> String
dropWord = dropWhile (/= ' ') . dropWhile (== ' ')

stripBegin :: String -> String
stripBegin = dropWhile (== ' ')

withDocInitialize :: a -> (String, a)
withDocInitialize a = ("", a)

haddockParseFold ::
       Monad m
    => t3
    -> t2
    -> t1
    -> Fold m String (API (Attach Doc t3) (Attach Doc t2) (Attach Doc t1))
haddockParseFold tag3 tag2 tag1 = extract <$> Fold.foldl' step initial

    where

    mapperStr doc = Tagged (Attach doc tag1) . ECString

    mapperFix doc = Tagged (Attach doc tag1) . ECFixity

    mCtxAp doc api name ctx = Map.insert name (Tagged (Attach doc tag3) ctx) api

    dtCtxAp doc ctx dname dctx =
        ctx
            { mDataTypes =
                  Map.insert
                      dname
                      (Tagged (Attach doc tag2) dctx)
                      (mDataTypes ctx)
            }

    tCtxAp doc ctx dname dctx =
        ctx
            { mNewTypes =
                  Map.insert
                      dname
                      (Tagged (Attach doc tag2) dctx)
                      (mNewTypes ctx)
            }

    extract (_, PNone api) = api
    extract (_, InModuleContext doc2 api name ctx) = mCtxAp doc2 api name ctx
    extract (_, InDataTypeContext doc2 doc1 api mname mctx dname dctx) =
        mCtxAp doc2 api mname (dtCtxAp doc1 mctx dname dctx)
    extract (_, InNewTypeContext doc2 doc1 api mname mctx dname dctx) =
        mCtxAp doc2 api mname (tCtxAp doc1 mctx dname dctx)

    initial = ("", PNone Map.empty)
    getAfterTypeCtx = reverse . takeWhile (/= '>') . reverse
    getW1AfterTypeCtx = takeWord . getAfterTypeCtx
    getRestAfterTypeCtx = stripBegin . dropWord . getAfterTypeCtx
    getClassName = getW1AfterTypeCtx
    getInstanceOf = getW1AfterTypeCtx
    getInstanceFor = getRestAfterTypeCtx
    getRestAfterW2 = stripBegin . dropWord . dropWord
    getW2 = takeWord . dropWord

    stepOtherScope doc2 doc1 doc constr applier api name ctx dname dctx line
        | isUpper (head line) || (head line == '[') =
            let newDCtx = Map.insert (takeWord line) (mapperStr doc line) dctx
             in withDocInitialize $ constr doc2 doc1 api name ctx dname newDCtx
        | otherwise =
            let newCtx = applier doc1 ctx dname dctx
             in step_ doc (InModuleContext doc2 api name newCtx) line

    stepModuleScope doc2 doc api name ctx line
        | "module " `isPrefixOf` line =
            withDocInitialize
                $ InModuleContext
                      doc
                      (mCtxAp doc2 api name ctx)
                      (getW2 line)
                      defModuleContext
        | "class " `isPrefixOf` line =
            withDocInitialize
                $ InModuleContext doc2 api name
                $ ctx
                      { mClasses =
                            Map.insert
                                (getClassName line)
                                (mapperStr doc line)
                                (mClasses ctx)
                      }
        | "instance " `isPrefixOf` line =
            let singMap =
                    Tagged (Attach "" tag2)
                        $ Map.singleton
                              (getInstanceOf line)
                              (mapperStr doc line)

                insertF (Tagged (Attach "" _) a) (Tagged (Attach "" _) b) =
                    Tagged (Attach "" tag2) (SMap.union a b)
                insertF _ _ = error "Instance level 1 has doc"
             in withDocInitialize
                    $ InModuleContext doc2 api name
                    $ ctx
                          { mInstances =
                                SMap.insertWith
                                    insertF
                                    (getInstanceFor line)
                                    singMap
                                    (mInstances ctx)
                          }
        | "data " `isPrefixOf` line =
            withDocInitialize
                $ InDataTypeContext doc2 doc api name ctx (getW2 line) Map.empty
        | "type " `isPrefixOf` line =
            withDocInitialize
                $ InModuleContext doc2 api name
                $ ctx
                      { mTypeAliases =
                            Map.insert
                                (getW2 line)
                                (mapperStr doc line)
                                (mTypeAliases ctx)
                      }
        | "newtype " `isPrefixOf` line =
            withDocInitialize
                $ InNewTypeContext doc2 doc api name ctx (getW2 line) Map.empty
        | "infixl " `isPrefixOf` line =
            withDocInitialize
                $ InModuleContext doc2 api name
                $ ctx
                      { mFixities =
                            Map.insert
                                (getRestAfterW2 line)
                                (mapperFix doc (FixityL, read (getW2 line)))
                                (mFixities ctx)
                      }
        | "infixr " `isPrefixOf` line =
            withDocInitialize
                $ InModuleContext doc2 api name
                $ ctx
                      { mFixities =
                            Map.insert
                                (getRestAfterW2 line)
                                (mapperFix doc (FixityR, read (getW2 line)))
                                (mFixities ctx)
                      }
        | "infix " `isPrefixOf` line =
            withDocInitialize
                $ InModuleContext doc2 api name
                $ ctx
                      { mFixities =
                            Map.insert
                                (getRestAfterW2 line)
                                (mapperFix doc (FixityD, read (getW2 line)))
                                (mFixities ctx)
                      }
        | "pattern " `isPrefixOf` line =
            withDocInitialize
                $ InModuleContext doc2 api name
                $ ctx
                      { mPatterns =
                            Map.insert
                                (getW2 line)
                                (mapperStr doc line)
                                (mPatterns ctx)
                      }
        | otherwise =
            withDocInitialize
                $ InModuleContext doc2 api name
                $ ctx
                      { mFunctions =
                            Map.insert
                                (takeWord line)
                                (mapperStr doc line)
                                (mFunctions ctx)
                      }

    step_ doc st line
        | "@" `isPrefixOf` line = ("", st)
        | "--" `isPrefixOf` line = (unlines [doc, line], st)
        | null line = (unlines [doc, line], st)
        | line == "\n" = (unlines [doc, line], st)
        | otherwise =
            case st of
                -- Shift to a new module context from nothing
                PNone api ->
                    if "module" `isPrefixOf` line
                    then withDocInitialize
                             $ InModuleContext
                                   doc
                                   api
                                   (drop 7 line)
                                   defModuleContext
                    else (doc, PNone api)
                InModuleContext doc2 api name ctx ->
                    stepModuleScope doc2 doc api name ctx line
                InDataTypeContext doc2 doc1 api mname mctx dname dctx ->
                    stepOtherScope
                        doc2
                        doc1
                        doc
                        InDataTypeContext
                        dtCtxAp
                        api
                        mname
                        mctx
                        dname
                        dctx
                        line
                InNewTypeContext doc2 doc1 api mname mctx dname dctx ->
                    stepOtherScope
                        doc2
                        doc1
                        doc
                        InNewTypeContext
                        tCtxAp
                        api
                        mname
                        mctx
                        dname
                        dctx
                        line

    step (doc, st) = step_ doc st
