-- |
-- Module      : PackDiff.Pretty
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Rendering of API diffs and APIs as text.

module PackDiff.Pretty
    ( -- * Lines
      Style (..)
    , Seg (..)
    , Line (..)
    , renderLines

      -- * Diff
    , GroupBy (..)
    , RenderOptions (..)
    , defaultRenderOptions
    , renderDiff
    , summaryLines

      -- * API
    , renderAPI

      -- * Helpers
    , highlightPair
    ) where

import Data.Char (isAlphaNum, isSpace, toUpper)
import Data.List (intercalate, isPrefixOf)
import Data.Map (Map)
import System.Console.ANSI
    ( Color(..)
    , ColorIntensity(..)
    , ConsoleLayer(..)
    , SGR(..)
    , setSGRCode
    )

import qualified Data.Map.Strict as Map

import PackDiff.API
import PackDiff.Classify (breakingCount)
import PackDiff.Diff

--------------------------------------------------------------------------------
-- Lines
--------------------------------------------------------------------------------

data Style
    = StyleAdded
    | StyleRemoved
    | StyleChanged
    | StyleDeprecated
    | StylePlain
    deriving (Eq, Show)

-- | A part of a line. Highlighted parts are the differing parts of a pair of
-- @-@ and @+@ lines.
data Seg
    = Plain String
    | Highlight String
    deriving (Eq, Show)

data Line = Line
    { lineLevel :: Int -- ^ nesting level, 4 spaces each
    , lineStyle :: Style
    , lineSegs :: [Seg]
    } deriving (Eq, Show)

blank :: Line
blank = Line 0 StylePlain []

plainLine :: Int -> Style -> String -> Line
plainLine lvl st s = Line lvl st [Plain s]

styleSGR :: Style -> [SGR]
styleSGR st =
    case st of
        StyleAdded -> [SetColor Foreground Dull Green]
        StyleRemoved -> [SetColor Foreground Dull Red]
        StyleChanged -> [SetColor Foreground Dull Yellow]
        StyleDeprecated -> [SetColor Foreground Dull Magenta]
        StylePlain -> []

renderLine :: Bool -> Line -> String
renderLine color (Line lvl st segs) =
    replicate (4 * lvl) ' ' ++ body

    where

    sgr = styleSGR st
    seg (Plain s) = s
    seg (Highlight s)
        | color = setSGRCode [SetSwapForegroundBackground True] ++ s
            ++ setSGRCode [SetSwapForegroundBackground False]
        | otherwise = s
    text = concatMap seg segs
    body
        | color && not (null sgr) && not (null text) =
            setSGRCode sgr ++ text ++ setSGRCode [Reset]
        | otherwise = text

-- | Render lines, with ANSI colors if the first argument is True.
renderLines :: Bool -> [Line] -> String
renderLines color = unlines . map (renderLine color)

--------------------------------------------------------------------------------
-- Changes
--------------------------------------------------------------------------------

sigil :: Change -> String
sigil c =
    case c of
        Added -> "[+]"
        Removed -> "[-]"
        Changed -> "[~]"
        Deprecated -> "[D]"
        Undeprecated -> "[U]"

changeStyle :: Change -> Style
changeStyle c =
    case c of
        Added -> StyleAdded
        Removed -> StyleRemoved
        Changed -> StyleChanged
        Deprecated -> StyleDeprecated
        Undeprecated -> StyleDeprecated

changeName :: Change -> String
changeName c =
    case c of
        Added -> "added"
        Removed -> "removed"
        Changed -> "changed"
        Deprecated -> "deprecated"
        Undeprecated -> "undeprecated"

sigilLine :: Int -> Change -> String -> Line
sigilLine lvl c s = plainLine lvl (changeStyle c) (sigil c ++ " " ++ s)

--------------------------------------------------------------------------------
-- Highlighting
--------------------------------------------------------------------------------

-- | Identifiers, runs of spaces and single other characters.
tokens :: String -> [String]
tokens [] = []
tokens s@(c : cs)
    | isIdent c = let (a, b) = span isIdent s in a : tokens b
    | isSpace c = let (a, b) = span isSpace s in a : tokens b
    | otherwise = [c] : tokens cs

    where

    isIdent x = isAlphaNum x || x == '_' || x == '\''

-- XXX Use streamly-core's dropCommonPrefixBy when it is available in a
-- release (see dev/upstream-fixes.md).
dropCommonPrefix :: Eq a => [a] -> [a] -> ([a], [a], [a])
dropCommonPrefix (x : xs) (y : ys)
    | x == y = let (p, a, b) = dropCommonPrefix xs ys in (x : p, a, b)
dropCommonPrefix xs ys = ([], xs, ys)

-- | Split each line of a pair into the common prefix, the differing middle
-- and the common suffix, at token boundaries.
highlightPair :: String -> String -> ([Seg], [Seg])
highlightPair old new = (segs midOld, segs midNew)

    where

    (prefix, restOld, restNew) = dropCommonPrefix (tokens old) (tokens new)
    (rsuffix, rmidOld, rmidNew) =
        dropCommonPrefix (reverse restOld) (reverse restNew)
    suffix = reverse rsuffix
    midOld = reverse rmidOld
    midNew = reverse rmidNew
    segs mid =
        filter nonEmpty
            [Plain (concat prefix), Highlight (concat mid), Plain (concat suffix)]
    nonEmpty (Plain s) = not (null s)
    nonEmpty (Highlight s) = not (null s)

-- | The @-@ and @+@ lines of a changed definition.
definitionPair :: Int -> String -> String -> [Line]
definitionPair lvl old new =
    [ Line lvl StyleRemoved (Plain "- " : o)
    , Line lvl StyleAdded (Plain "+ " : n)
    ]

    where

    (o, n) = highlightPair old new

--------------------------------------------------------------------------------
-- Entity lines
--------------------------------------------------------------------------------

-- | Adds the module name to an entity line, used when the output is grouped
-- by change.
type Qualify = Decl -> String -> String

-- | The text of a @[~]@ line: the name of a function, pattern or fixity, the
-- declaration head of a data type, newtype or class.
declLabel :: Decl -> String
declLabel d =
    case declKind d of
        KData -> declText d
        KClass -> declText d
        KTypeSyn -> takeBefore " = " (declText d)
        KInstance -> declText d
        KPattern -> declName d
        KFunction -> declName d
        KFixity -> declName d

    where

    takeBefore sep str@(c : cs)
        | sep `isPrefixOf` str = []
        | otherwise = c : takeBefore sep cs
    takeBefore _ [] = []

childLines :: Int -> ChildDiff -> [Line]
childLines lvl cd =
    case (cdChange cd, cdOld cd, cdNew cd) of
        (Changed, Just o, Just n) ->
            sigilLine lvl Changed (childName n)
                : definitionPair (lvl + 1) (childText o) (childText n)
        (c, _, Just n) -> [sigilLine lvl c (childText n)]
        (c, Just o, Nothing) -> [sigilLine lvl c (childText o)]
        (_, Nothing, Nothing) -> []

-- | Lines of a declaration and its visible children. Empty if neither the
-- declaration nor any child is visible.
declLines :: (Change -> Bool) -> Qualify -> Int -> DeclDiff -> [Line]
declLines visible qual lvl dd
    | visible (ddChange dd) || not (null children) = self ++ children
    | otherwise = []

    where

    d = declDiffDecl dd
    children =
        concatMap (childLines (lvl + 1))
            $ filter (visible . cdChange) (ddChildren dd)
    self =
        case (ddChange dd, ddOld dd, ddNew dd) of
            (Changed, Just o, Just n) ->
                sigilLine lvl Changed (qual n (declLabel n))
                    : if declText o /= declText n
                      then definitionPair (lvl + 1) (declText o) (declText n)
                      else []
            (c, _, _) -> [sigilLine lvl c (qual d (declText d))]

-- | The declarations of a module and their children, all marked with the
-- same sigil.
moduleSymbolLines :: Change -> Int -> Module -> [Line]
moduleSymbolLines c lvl m = concatMap decl (Map.elems (moduleDecls m))

    where

    decl d =
        sigilLine lvl c (declText d)
            : map (sigilLine (lvl + 1) c . childText) (declChildren d)

symbols :: Int -> String
symbols 1 = "(1 symbol)"
symbols n = "(" ++ show n ++ " symbols)"

-- | The module line. Added and removed modules show their size.
moduleLine :: Int -> ModuleName -> ModuleDiff -> Line
moduleLine lvl name md =
    case mdChange md of
        c | c `elem` [Added, Removed] ->
            sigilLine lvl c
                (name ++ " " ++ symbols (moduleSize (moduleDiffModule md)))
        c -> sigilLine lvl c name

--------------------------------------------------------------------------------
-- Diff
--------------------------------------------------------------------------------

data GroupBy
    = GroupByModule
    | GroupByChange
    deriving (Eq, Show)

data RenderOptions = RenderOptions
    { roGroupBy :: GroupBy
    , roQuiet :: Bool
    , roCollapse :: Bool
      -- ^ Print only the module lines.
    , roExpand :: Bool
      -- ^ List the symbols of added and removed modules.
    , roVisible :: Change -> Bool
    }

defaultRenderOptions :: RenderOptions
defaultRenderOptions =
    RenderOptions
        { roGroupBy = GroupByModule
        , roQuiet = False
        , roCollapse = False
        , roExpand = False
        , roVisible = const True
        }

-- | The module line of an added or removed module, followed by its symbols
-- with @--expand@.
addedRemovedLines :: RenderOptions -> ModuleName -> ModuleDiff -> [Line]
addedRemovedLines ro name md =
    moduleLine 0 name md
        : if roExpand ro
          then moduleSymbolLines (mdChange md) 1 (moduleDiffModule md)
          else []

-- | Lines of one module in the output grouped by module.
moduleLines :: RenderOptions -> ModuleName -> ModuleDiff -> [Line]
moduleLines ro name md =
    case mdChange md of
        c | c `elem` [Added, Removed] ->
            if visible c then addedRemovedLines ro name md else []
        c | visible c && c /= Changed || not (null decls) ->
            moduleLine 0 name md : if roCollapse ro then [] else decls
        _ -> []

    where

    visible = roVisible ro
    decls = concatMap (declLines visible (\_ s -> s) 1) (mdDecls md)

groupedByModule :: RenderOptions -> APIDiff -> [Line]
groupedByModule ro = concatMap (uncurry (moduleLines ro)) . Map.toList

-- | Qualify the name of an entity with its module name.
qualifyWith :: ModuleName -> Qualify
qualifyWith m d s =
    case declKind d of
        KInstance -> m ++ ": " ++ s
        _ -> replaceFirstWord (declName d) s

    where

    qualified name =
        case name of
            ('(' : op) -> "(" ++ m ++ "." ++ op
            _ -> m ++ "." ++ name
    replaceFirstWord name str =
        case break (== name) (splitKeep str) of
            (before, _ : after) -> concat before ++ qualified name ++ concat after
            (_, []) -> m ++ ": " ++ str
    -- words with the spaces kept as separate elements
    splitKeep [] = []
    splitKeep str@(c : _)
        | isSpace c = let (a, b) = span isSpace str in a : splitKeep b
        | otherwise = let (a, b) = break isSpace str in a : splitKeep b

groupedByChange :: RenderOptions -> APIDiff -> [Line]
groupedByChange ro diff =
    intercalate [blank] (filter (not . null) (map section [minBound .. maxBound]))

    where

    visible = roVisible ro
    mods = Map.toList diff
    section c
        | null items = []
        | otherwise = plainLine 0 StylePlain (capitalize (changeName c) ++ ":") : items

        where

        modItems =
            [ l
            | visible c, c /= Changed, (name, md) <- mods, mdChange md == c
            , l <- addedRemovedLines ro name md
            ]
        declItems =
            [ l
            | not (roCollapse ro)
            , (name, md) <- mods
            , dd <- mdDecls md
            , ddChange dd == c
            , l <- declLines visible (qualifyWith name) 0 dd
            ]
        items = modItems ++ declItems
    capitalize (x : xs) = toUpper x : xs
    capitalize [] = []

countLine :: String -> [Change] -> [String]
countLine label cs
    | null parts = []
    | otherwise = [label ++ ": " ++ intercalate ", " parts]

    where

    order = [Added, Removed, Changed, Deprecated, Undeprecated]
    count c = length (filter (== c) cs)
    parts =
        [ show (count c) ++ " " ++ changeName c ++ " " ++ sigil c
        | c <- order
        , count c > 0
        ]

-- | The summary of a diff of released and internal modules.
summaryLines :: APIDiff -> APIDiff -> [String]
summaryLines released internal
    | Map.null released && Map.null internal = ["No API changes."]
    | otherwise =
        countLine "Modules" (map mdChange rel)
            ++ countLine "Symbols" symbolChanges
            ++ countLine "Internal modules" (map mdChange (Map.elems internal))
            ++ [ "Breaking changes: " ++ show breaking | breaking > 0 ]

    where

    rel = Map.elems released
    breaking = breakingCount rel
    symbolChanges =
        [ ch
        | md <- rel
        , mdChange md `notElem` [Added, Removed]
        , dd <- mdDecls md
        , ch <- ddChange dd : map cdChange (ddChildren dd)
        ]

-- | Render the diff of released and internal modules followed by the summary.
renderDiff :: RenderOptions -> APIDiff -> APIDiff -> [Line]
renderDiff ro released internal =
    body ++ [blank | not (null body)] ++ summary

    where

    summary = map (plainLine 0 StylePlain) (summaryLines released internal)
    group =
        case roGroupBy ro of
            GroupByModule -> groupedByModule ro
            GroupByChange -> groupedByChange ro
    rel = group released
    int = group internal
    internalSection
        | null int = []
        | otherwise =
            [blank | not (null rel)]
                ++ plainLine 0 StylePlain "Internal modules:" : int
    body
        | roQuiet ro = []
        | otherwise = rel ++ internalSection

--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------

apiModuleLines :: ModuleName -> Module -> [Line]
apiModuleLines name m =
    sigilLine 0 Added name : moduleSymbolLines Added 1 m

-- | The API of released and internal modules in the tree format, with every
-- entity marked @[+]@.
renderAPI :: Map ModuleName Module -> Map ModuleName Module -> [Line]
renderAPI released internal = rel ++ internalSection

    where

    linesOf = concatMap (uncurry apiModuleLines) . Map.toList
    rel = linesOf released
    int = linesOf internal
    internalSection
        | null int = []
        | otherwise =
            [blank | not (null rel)]
                ++ plainLine 0 StylePlain "Internal modules:" : int
