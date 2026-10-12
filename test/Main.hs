-- |
-- Module      : Main
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- The golden tests compare the output with the files in @test/golden@. Run
-- with @PACKDIFF_ACCEPT=1@ to write the current output to them instead.

module Main (main) where

import Data.Maybe (isJust)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import Test.Hspec

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Streamly.FileSystem.Path as Path

import PackDiff.API
import PackDiff.Cabal (hoogleFileFromOutput, latestVersion, parseVersionList)
import PackDiff.Cache (CacheKey(..), cacheFileName)
import PackDiff.Classify
import PackDiff.Diff
import PackDiff.Filter
import PackDiff.Git (matchRemoteRef)
import PackDiff.Hash (fnv1a64, hashHex)
import PackDiff.HoogleFileParser
import PackDiff.Main (defaultRefs)
import PackDiff.Options
import PackDiff.Pretty
import PackDiff.Ref
import PackDiff.WorkDir (parsePidPrefix)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

dataFile :: String -> FilePath
dataFile name = "test/data/" ++ name

readHoogle :: String -> IO Hoogle
readHoogle = readHoogleFile . Path.fromString_ . dataFile

-- | Parse a file given as a list of lines, failing the test on an error.
parseLines :: [String] -> Hoogle
parseLines = either error id . parseHoogleLines

-- | Compare with a golden file, or write it when PACKDIFF_ACCEPT is set.
golden :: String -> String -> Expectation
golden name actual = do
    let path = "test/golden/" ++ name ++ ".out"
    accept <- lookupEnv "PACKDIFF_ACCEPT"
    if isJust accept
    then writeFile path actual
    else do
        expected <- readFile path
        actual `shouldBe` expected

internalFilter :: ModuleFilter
internalFilter = ModuleFilter [] [] defaultInternalGlobs

-- | Diff the two test files, split into released and internal modules.
testDiff :: IO (APIDiff, APIDiff)
testDiff = do
    old <- readHoogle "old.txt"
    new <- readHoogle "new.txt"
    pure $ partitionInternal internalFilter
        $ diffAPI (hoogleAPI old) (hoogleAPI new)

renderTestDiff :: RenderOptions -> IO String
renderTestDiff ro = do
    (rel, int) <- testDiff
    pure $ renderLines False (renderDiff ro rel int)

declsOf :: ModuleName -> Hoogle -> [Decl]
declsOf m h =
    maybe [] (Map.elems . moduleDecls) (Map.lookup m (hoogleAPI h))

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------

parserSpec :: Spec
parserSpec = describe "HoogleFileParser" $ do
    it "reads @package and @version lines" $ do
        h <- readHoogle "old.txt"
        hooglePackages h `shouldBe` ["testpkg"]
        hoogleVersions h `shouldBe` ["1.2.0.0"]

    it "groups constructors and fields under their data type" $ do
        h <- readHoogle "old.txt"
        let fold = [d | d <- declsOf "Test.Fold" h, declName d == "Fold"]
        map (map childName . declChildren) fold
            `shouldBe` [["Fold", "[foldStep]"]]

    it "joins a multi line class declaration and finds its methods" $ do
        h <- readHoogle "old.txt"
        let cls = [d | d <- declsOf "Test.Fold" h, declName d == "IsMap"]
        map declText cls
            `shouldBe`
                [ "class IsMap (f :: Type -> Type) where { type Key (f :: \
                  \Type -> Type); }"
                ]
        map (map childName . declChildren) cls
            `shouldBe` [["mapEmpty", "mapInsert"]]

    it "takes the class name after the context" $ do
        h <- readHoogle "old.txt"
        [declName d | d <- declsOf "Test.Fold" h, declKind d == KClass]
            `shouldBe` ["IsMap", "MonadAsync"]

    it "marks deprecated declarations and modules" $ do
        h <- readHoogle "new.txt"
        [declName d | d <- declsOf "Test.Prelude" h, declDeprecated d]
            `shouldBe` ["parEval"]
        fmap moduleDeprecated (Map.lookup "Test.ToBeDeprecated" (hoogleAPI h))
            `shouldBe` Just True

    it "parses fixities and operators" $ do
        h <- readHoogle "old.txt"
        [declName d | d <- declsOf "Test.Fold" h, declKind d == KFixity]
            `shouldBe` ["<>"]
        [declName d | d <- declsOf "Test.Fold" h, declName d == "(<>)"]
            `shouldBe` ["(<>)"]

    it "skips a forall when matching class methods" $ do
        let h = parseLines
                [ "module M"
                , "class C m"
                , "f :: forall (m :: Type -> Type) a. C m => a -> m a"
                , "g :: forall a. Show a => a"
                ]
        map (map childName . declChildren) (filter ((== KClass) . declKind) (declsOf "M" h))
            `shouldBe` [["f"]]

    it "rejects a declaration outside a module" $
        parseHoogleLines ["foo :: Int"] `shouldSatisfy` either (const True) (const False)

    it "parses a concatenation as the merge of the parts" $ do
        a <- readFile (dataFile "pkga.txt")
        b <- readFile (dataFile "pkgb.txt")
        let ha = parseLines (lines a)
            hb = parseLines (lines b)
            hab = parseLines (lines (a ++ b))
        hoogleAPI hab
            `shouldBe` mergeNonConflictingAPI (hoogleAPI ha) (hoogleAPI hb)
        hooglePackages hab `shouldBe` ["pkga", "pkgb"]
        hoogleVersions hab `shouldBe` ["0.1", "0.2"]

    it "replaces a repeated module with the later one" $ do
        let h = parseLines ["module M", "a :: Int", "module M", "b :: Int"]
        map declName (declsOf "M" h) `shouldBe` ["b"]

--------------------------------------------------------------------------------
-- Diff and classification
--------------------------------------------------------------------------------

diffSpec :: Spec
diffSpec = describe "Diff" $ do
    it "classifies module changes" $ do
        (rel, int) <- testDiff
        Map.map mdChange rel
            `shouldBe` Map.fromList
                [ ("Test.Fold", Changed)
                , ("Test.Prelude", Changed)
                , ("Test.Scanl", Added)
                , ("Test.ToBeDeprecated", Deprecated)
                , ("Test.Unfold.Old", Removed)
                ]
        Map.keys int `shouldBe` ["Test.Internal.Fold"]

    it "counts breaking changes" $ do
        (rel, _) <- testDiff
        -- module removed; Step, Fold, IsMap, toHashMapIO, removedFn,
        -- instance Show, fixity <> changed or removed
        breakingCount (Map.elems rel) `shouldBe` 8

    it "does not count a child deprecation as breaking" $ do
        let child c = Child c c
            decl = Decl KData "T" "data T" [] False
            dd = DeclDiff Changed (Just (decl [child "A" False]))
                    (Just (decl [child "A" True]))
                    [ChildDiff Deprecated (Just (child "A" False)) (Just (child "A" True))]
        isBreakingDecl dd `shouldBe` False

    it "requires bumps according to the PVP" $ do
        (rel, _) <- testDiff
        requiredBump (Map.elems rel) `shouldBe` BumpMajor
        requiredBump [] `shouldBe` BumpPatch
        bumpVersion BumpMajor [1, 2] `shouldBe` [1, 3, 0, 0]
        bumpVersion BumpMinor [1, 2, 0, 0] `shouldBe` [1, 2, 1, 0]
        bumpVersion BumpPatch [1, 2, 0, 0] `shouldBe` [1, 2, 0, 1]

    it "compares versions with missing components as zero" $ do
        compareVersion [1, 2] [1, 2, 0, 0] `shouldBe` EQ
        compareVersion [1, 2, 1] [1, 3, 0, 0] `shouldBe` LT
        parseVersion "1.2.0.1" `shouldBe` Just [1, 2, 0, 1]
        parseVersion "1.x" `shouldBe` Nothing

    it "evaluates --fail-on" $ do
        (rel, int) <- testDiff
        let failOn s = either error (`hasChangeIn` Map.elems rel) (parseFailOn s)
        failOn "breaking" `shouldBe` True
        failOn "undeprecated" `shouldBe` True
        -- internal modules never count
        either error (`hasChangeIn` Map.elems int) (parseFailOn "added")
            `shouldBe` True
        parseFailOn "bogus" `shouldSatisfy` either (const True) (const False)

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

prettySpec :: Spec
prettySpec = describe "Pretty" $ do
    it "renders the diff grouped by module" $
        renderTestDiff defaultRenderOptions >>= golden "diff"

    it "renders the diff grouped by change" $
        renderTestDiff defaultRenderOptions { roGroupBy = GroupByChange }
            >>= golden "diff-group-by-change"

    it "renders only shown change types" $
        renderTestDiff defaultRenderOptions { roVisible = (== Added) }
            >>= golden "diff-show-added"

    it "renders module lines only" $
        renderTestDiff defaultRenderOptions { roCollapse = True }
            >>= golden "diff-collapse"

    it "renders the symbols of added and removed modules" $
        renderTestDiff defaultRenderOptions { roExpand = True }
            >>= golden "diff-expand"

    it "renders the symbols of added and removed modules grouped by change" $
        renderTestDiff
            defaultRenderOptions { roExpand = True, roGroupBy = GroupByChange }
            >>= golden "diff-expand-group-by-change"

    it "renders only the summary when quiet" $
        renderTestDiff defaultRenderOptions { roQuiet = True }
            >>= golden "diff-quiet"

    it "renders an API" $ do
        h <- readHoogle "new.txt"
        let (rel, int) = partitionInternal internalFilter (hoogleAPI h)
        golden "api" (renderLines False (renderAPI rel int))

    it "renders an empty diff" $
        renderLines False (renderDiff defaultRenderOptions Map.empty Map.empty)
            `shouldBe` "No API changes.\n"

    it "highlights the differing tokens" $
        highlightPair "f :: (A, B) => C" "f :: (A) => C"
            `shouldBe`
                ( [Plain "f :: (A", Highlight ", B", Plain ") => C"]
                , [Plain "f :: (A", Plain ") => C"]
                )

    it "colors lines when asked" $
        renderLines True [Line 0 StyleAdded [Plain "[+] x"]]
            `shouldSatisfy` (/= "[+] x\n")

--------------------------------------------------------------------------------
-- Refs, options and helpers
--------------------------------------------------------------------------------

refSpec :: Spec
refSpec = describe "Ref" $ do
    it "parses refs" $ do
        parseRef "." `shouldBe` Right RefCwd
        parseRef "hackage" `shouldBe` Right (RefHackage HackageLatest)
        parseRef "hackage:latest" `shouldBe` Right (RefHackage HackageLatest)
        parseRef "hackage:1.2" `shouldBe` Right (RefHackage (HackageVersion "1.2"))
        parseRef "git:hackage" `shouldBe` Right (RefGit "hackage")
        parseRef "v1.2.0" `shouldBe` Right (RefGit "v1.2.0")
        parseRef "file:api.txt" `shouldBe` Right (RefFile "api.txt")
        parseRef "foo:bar" `shouldSatisfy` either (const True) (const False)
        parseRef "-x" `shouldSatisfy` either (const True) (const False)

    it "classifies repositories" $ do
        classifyRepo True "../repo" `shouldBe` Right (Left "../repo")
        classifyRepo False "https://github.com/a/b"
            `shouldBe` Right (Right "https://github.com/a/b")
        classifyRepo False "git@github.com:a/b" `shouldBe` Right (Right "git@github.com:a/b")
        classifyRepo False "nothing" `shouldSatisfy` either (const True) (const False)

    it "fills in default refs" $ do
        defaultRefs CmdDiff [] `shouldBe` Right ["HEAD", "."]
        defaultRefs CmdCheck ["v1"] `shouldBe` Right ["v1", "."]
        defaultRefs CmdApi [] `shouldBe` Right ["."]
        defaultRefs CmdApi ["a", "b"] `shouldSatisfy` either (const True) (const False)

    it "finds revisions in git ls-remote output" $ do
        let out =
                [ "1111111111111111111111111111111111111111\tHEAD"
                , "2222222222222222222222222222222222222222\trefs/heads/master"
                , "3333333333333333333333333333333333333333\trefs/tags/v1"
                , "4444444444444444444444444444444444444444\trefs/tags/v1^{}"
                ]
        matchRemoteRef out "master" `shouldBe` Just (replicate 40 '2')
        matchRemoteRef out "v1" `shouldBe` Just (replicate 40 '4')
        matchRemoteRef out "HEAD" `shouldBe` Just (replicate 40 '1')
        matchRemoteRef out "v1~1" `shouldBe` Nothing

optionsSpec :: Spec
optionsSpec = describe "Options" $ do
    let parse = parseOptionsPure "packdiff"
    it "parses a diff command" $
        fmap (\o -> (optCommand o, optRefs o, optPackage o)) (either (Left . fst) Right (parse ["diff", "-p", "x", "v1", "v2"]))
            `shouldBe` Right (CmdDiff, ["v1", "v2"], Just "x")

    it "combines comma separated and repeated change types" $
        fmap optShow (either (Left . fst) Right (parse ["diff", "--show", "added,removed", "--show", "changed"]))
            `shouldBe` Right (Just (Set.fromList [Added, Removed, Changed]))

    it "exits with 2 on a usage error" $
        either (Just . snd) (const Nothing) (parse ["diff", "--bogus"])
            `shouldBe` Just (ExitFailure 2)

    it "does not accept --fail-on for api" $
        either (Just . snd) (const Nothing) (parse ["api", "--fail-on", "any"])
            `shouldBe` Just (ExitFailure 2)

helpersSpec :: Spec
helpersSpec = describe "Helpers" $ do
    it "matches globs" $ do
        globMatch "*.Internal.*" "Streamly.Internal.Data.Fold" `shouldBe` True
        globMatch "*.Internal" "Streamly.Data.Internal" `shouldBe` True
        globMatch "*.Internal" "Streamly.Internal.Data" `shouldBe` False
        globMatch "Streamly.Data.*" "Streamly.Data.Fold" `shouldBe` True
        globMatch "A" "AB" `shouldBe` False

    it "hashes with FNV-1a" $ do
        fnv1a64 "" `shouldBe` 0xcbf29ce484222325
        fnv1a64 "a" `shouldBe` 0xaf63dc4c8601ec8c
        hashHex "" `shouldBe` "cbf29ce484222325"

    it "names cache entries" $
        cacheFileName (CacheKey "streamly" "abc" "9.10.3" "2.31.1" [])
            `shouldBe` "streamly-abc-ghc-9.10.3-haddock-2.31.1-"
                ++ hashHex "[]" ++ ".txt"

    it "parses work directory names" $ do
        parsePidPrefix "1234-abc" `shouldBe` Just 1234
        parsePidPrefix "abc" `shouldBe` Nothing

    it "finds the hoogle file in cabal output" $ do
        hoogleFileFromOutput "Documentation created:\n/a/b/p.txt\n"
            `shouldBe` Just "/a/b/p.txt"
        hoogleFileFromOutput "Documentation created: dist/p/index.html,\ndist/p/p.txt\n"
            `shouldBe` Just "dist/p/p.txt"
        hoogleFileFromOutput "nothing here\n" `shouldBe` Nothing

    it "parses cabal list output" $ do
        let out = "streamly 0.10.1\nstreamly 0.9.0\nstreamly-core 0.2.2\n"
            vs = parseVersionList "streamly" out
        vs `shouldBe` [[0, 10, 1], [0, 9, 0]]
        latestVersion vs `shouldBe` Just [0, 10, 1]

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
    parserSpec
    diffSpec
    prettySpec
    refSpec
    optionsSpec
    helpersSpec
