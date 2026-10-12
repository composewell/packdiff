{-# LANGUAGE LambdaCase #-}
-- |
-- Module      : PackDiff.Options
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Command line options.

module PackDiff.Options
    ( Command (..)
    , ColorMode (..)
    , Format (..)
    , Options (..)
    , parserInfo
    , parseOptions
    , parseOptionsPure
    ) where

import Data.Set (Set)
import Options.Applicative
import Options.Applicative.Help.Pretty (vsep, pretty)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr)

import qualified Data.Set as Set

import PackDiff.Diff (Change)
import PackDiff.Filter (Glob, parseChangeTypes, parseFailOn)
import PackDiff.Pretty (GroupBy(..))

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data Command
    = CmdDiff
    | CmdCheck
    | CmdApi
    deriving (Eq, Show)

data ColorMode
    = ColorAuto
    | ColorAlways
    | ColorNever
    deriving (Eq, Show)

data Format
    = FormatText
    | FormatHoogle
    deriving (Eq, Show)

data Options = Options
    { optCommand :: Command
    , optRefs :: [String]
      -- Refs
    , optRepo :: Maybe String
    , optOldRepo :: Maybe String
    , optNewRepo :: Maybe String
    , optPackage :: Maybe String
      -- Output
    , optColor :: ColorMode
    , optGroupBy :: GroupBy
    , optQuiet :: Bool
    , optModulesOnly :: Bool
    , optFormat :: Format
      -- Filtering
    , optShow :: Maybe (Set Change)
    , optHide :: Maybe (Set Change)
    , optBreakingOnly :: Bool
    , optModules :: [Glob]
    , optIgnoreModules :: [Glob]
    , optInternalModules :: [Glob]
    , optFailOn :: Maybe (Set Change)
      -- Build
    , optCompiler :: Maybe String
    , optCabalOptions :: [String]
    , optNoCache :: Bool
    , optWorkDir :: Maybe FilePath
    , optInPlace :: Bool
    }

--------------------------------------------------------------------------------
-- Option groups
--------------------------------------------------------------------------------

-- | A repeatable option whose values are comma separated change type sets.
changeSets ::
    (String -> Either String (Set Change)) -> Mod OptionFields (Set Change)
    -> Parser (Maybe (Set Change))
changeSets parse m =
    fmap combine (many (option (eitherReader parse) m))

    where

    combine [] = Nothing
    combine xs = Just (Set.unions xs)

data RepoOpts = RepoOpts (Maybe String) (Maybe String) (Maybe String)

repoOpt :: Parser (Maybe String)
repoOpt =
    optional $ strOption $
        long "repo" <> metavar "PATH-OR-URL"
            <> help "Repository for git refs. Default: the repository of the \
                    \current directory."

oldNewRepoOpts :: Parser (Maybe String, Maybe String)
oldNewRepoOpts =
    (,)
        <$> optional (strOption $
                long "old-repo" <> metavar "PATH-OR-URL"
                    <> help "Repository for REF1 only.")
        <*> optional (strOption $
                long "new-repo" <> metavar "PATH-OR-URL"
                    <> help "Repository for REF2 only.")

packageOpt :: Parser (Maybe String)
packageOpt =
    optional $ strOption $
        short 'p' <> long "package" <> metavar "NAME"
            <> help "Package to compare, for repositories with several packages."

data ModuleOpts = ModuleOpts [Glob] [Glob] [Glob]

moduleOpts :: Parser ModuleOpts
moduleOpts =
    ModuleOpts
        <$> many (strOption $
                long "module" <> metavar "GLOB"
                    <> help "Only modules matching the glob. Repeatable.")
        <*> many (strOption $
                long "ignore-module" <> metavar "GLOB"
                    <> help "Exclude modules matching the glob. Repeatable.")
        <*> many (strOption $
                long "internal-module" <> metavar "GLOB"
                    <> help "Treat matching modules as internal. Repeatable. \
                            \Default: *.Internal.* and *.Internal.")

data BuildOpts = BuildOpts (Maybe String) [String] Bool (Maybe FilePath) Bool

buildOpts :: Parser BuildOpts
buildOpts =
    BuildOpts
        <$> optional (strOption $
                short 'w' <> long "with-compiler" <> metavar "GHC"
                    <> help "Compiler used to build each ref.")
        <*> many (strOption $
                long "cabal-option" <> metavar "OPT"
                    <> help "Extra option for cabal haddock, for every ref \
                            \that is built. Repeatable.")
        <*> switch (long "no-cache"
                <> help "Do not read or write the hoogle file cache.")
        <*> optional (strOption $
                long "work-dir" <> metavar "PATH"
                    <> help "Directory for work clones and unpacked Hackage \
                            \sources. Default: $XDG_CACHE_HOME/packdiff/work.")
        <*> switch (long "in-place"
                <> help "Build git refs of the current repository by checking \
                        \them out in the current working tree.")

data OutputOpts =
    OutputOpts ColorMode GroupBy Bool Bool
        (Maybe (Set Change)) (Maybe (Set Change)) Bool

colorReader :: ReadM ColorMode
colorReader = eitherReader $ \case
    "auto" -> Right ColorAuto
    "always" -> Right ColorAlways
    "never" -> Right ColorNever
    _ -> Left "expected auto, always or never"

groupByReader :: ReadM GroupBy
groupByReader = eitherReader $ \case
    "module" -> Right GroupByModule
    "change" -> Right GroupByChange
    _ -> Left "expected module or change"

colorOpt :: Parser ColorMode
colorOpt =
    option colorReader $
        long "color" <> metavar "MODE" <> value ColorAuto
            <> help "auto (default), always or never. auto disables color \
                    \when NO_COLOR is set or stdout is not a terminal."

outputOpts :: Parser OutputOpts
outputOpts =
    OutputOpts
        <$> colorOpt
        <*> option groupByReader
                (long "group-by" <> metavar "G" <> value GroupByModule
                    <> help "module (default) or change.")
        <*> switch (short 'q' <> long "quiet" <> help "Print only the summary.")
        <*> switch (long "modules-only"
                <> help "Print only the module lines and the summary.")
        <*> changeSets parseChangeTypes
                (long "show" <> metavar "TYPES"
                    <> help "Show only these change types: added, removed, \
                            \changed, deprecated, undeprecated.")
        <*> changeSets parseChangeTypes
                (long "hide" <> metavar "TYPES" <> help "Hide these change types.")
        <*> switch (long "breaking-only"
                <> help "Shorthand for --show removed,changed.")

failOnOpt :: Parser (Maybe (Set Change))
failOnOpt =
    changeSets parseFailOn
        (long "fail-on" <> metavar "TYPES"
            <> help "Exit with 1 if the diff contains a change of these types. \
                    \Same values as --show, plus breaking and any.")

formatOpt :: Parser Format
formatOpt =
    option reader $
        long "format" <> metavar "FMT" <> value FormatText
            <> help "text (default) or hoogle."

    where

    reader = eitherReader $ \case
        "text" -> Right FormatText
        "hoogle" -> Right FormatHoogle
        _ -> Left "expected text or hoogle"

refsArg :: String -> Parser [String]
refsArg meta = many (strArgument (metavar meta))

--------------------------------------------------------------------------------
-- Commands
--------------------------------------------------------------------------------

mkOptions ::
       Command
    -> [String]
    -> RepoOpts
    -> Maybe String
    -> ModuleOpts
    -> BuildOpts
    -> OutputOpts
    -> Maybe (Set Change)
    -> Format
    -> Options
mkOptions cmd refs (RepoOpts repo old new) pkg
    (ModuleOpts mods ignores internals)
    (BuildOpts compiler cabalOpts noCache workDir inPlace)
    (OutputOpts color groupBy quiet modsOnly shw hide breakingOnly)
    failOn fmt =
    Options
        { optCommand = cmd
        , optRefs = refs
        , optRepo = repo
        , optOldRepo = old
        , optNewRepo = new
        , optPackage = pkg
        , optColor = color
        , optGroupBy = groupBy
        , optQuiet = quiet
        , optModulesOnly = modsOnly
        , optFormat = fmt
        , optShow = shw
        , optHide = hide
        , optBreakingOnly = breakingOnly
        , optModules = mods
        , optIgnoreModules = ignores
        , optInternalModules = internals
        , optFailOn = failOn
        , optCompiler = compiler
        , optCabalOptions = cabalOpts
        , optNoCache = noCache
        , optWorkDir = workDir
        , optInPlace = inPlace
        }

defaultOutputOpts :: ColorMode -> OutputOpts
defaultOutputOpts color =
    OutputOpts color GroupByModule False False Nothing Nothing False

diffParser :: Parser Options
diffParser =
    (\refs repo (old, new) -> mkOptions CmdDiff refs (RepoOpts repo old new))
        <$> refsArg "[REF1 [REF2]]"
        <*> repoOpt
        <*> oldNewRepoOpts
        <*> packageOpt
        <*> moduleOpts
        <*> buildOpts
        <*> outputOpts
        <*> failOnOpt
        <*> pure FormatText

checkParser :: Parser Options
checkParser =
    (\refs repo (old, new) pkg mods bld ->
        mkOptions CmdCheck refs (RepoOpts repo old new) pkg mods bld
            (defaultOutputOpts ColorNever) Nothing FormatText)
        <$> refsArg "[REF1 [REF2]]"
        <*> repoOpt
        <*> oldNewRepoOpts
        <*> packageOpt
        <*> moduleOpts
        <*> buildOpts

apiParser :: Parser Options
apiParser =
    (\refs repo pkg mods bld color fmt ->
        mkOptions CmdApi refs (RepoOpts repo Nothing Nothing) pkg mods bld
            (defaultOutputOpts color) Nothing fmt)
        <$> refsArg "[REF]"
        <*> repoOpt
        <*> packageOpt
        <*> moduleOpts
        <*> buildOpts
        <*> colorOpt
        <*> formatOpt

usageFooter :: InfoMod a
usageFooter =
    footerDoc $ Just $ vsep $ map pretty
        [ "Refs: <rev> or git:<rev> (git revision), . (working directory),"
        , "hackage:<version>, hackage or hackage:latest, file:<path> (hoogle file)."
        , ""
        , "Sigils: [+] added, [-] removed, [~] changed, [D] deprecated,"
        , "[U] undeprecated. Below a [~] entity, - is the old definition and"
        , "+ is the new one."
        , ""
        , "Exit codes: 0 success, 1 --fail-on threshold reached or check found"
        , "the declared version insufficient, 2 usage error, 3 failure to"
        , "obtain an API."
        ]

commandParser :: Parser Options
commandParser =
    hsubparser $
        command "diff"
            (info diffParser
                (progDesc "Print the API diff between two refs." <> usageFooter))
        <> command "check"
            (info checkParser
                (progDesc "Print the version bump required by the diff." <> usageFooter))
        <> command "api"
            (info apiParser (progDesc "Print the API of a single ref." <> usageFooter))

parserInfo :: ParserInfo Options
parserInfo =
    info (commandParser <**> helper) $
        fullDesc
            <> progDesc "Find API changes between versions of a Haskell package."
            <> usageFooter

-- | Parse arguments without any IO. A failure carries the message and the
-- exit code: 0 for --help, 2 for a usage error.
parseOptionsPure :: String -> [String] -> Either (String, ExitCode) Options
parseOptionsPure prog args =
    case execParserPure defaultPrefs parserInfo args of
        Success opts -> Right opts
        Failure f ->
            let (msg, code) = renderFailure f prog
             in Left (msg, if code == ExitSuccess then code else ExitFailure 2)
        CompletionInvoked _ -> Left ("", ExitSuccess)

-- | Parse the command line, exiting on --help or a usage error.
parseOptions :: IO Options
parseOptions = do
    prog <- getProgName
    args <- getArgs
    case execParserPure defaultPrefs parserInfo args of
        Failure f
            | (msg, ExitFailure _) <- renderFailure f prog ->
                hPutStrLn stderr msg >> exitWith (ExitFailure 2)
        r -> handleParseResult r
