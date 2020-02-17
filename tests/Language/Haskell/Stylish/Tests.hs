--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Data.List                           (sort)
import           System.Directory                    (createDirectory)
import           System.FilePath                     (normalise, (</>))
import           Test.Framework                      (Test, testGroup)
import           Test.Framework.Providers.HUnit      (testCase)
import           Test.HUnit                          (Assertion, (@?=))


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish
import           Language.Haskell.Stylish.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Language.Haskell.Stylish.Step.Tabs.Tests"
    [ testCase "case 01" case01
    , testCase "case 02" case02
    , testCase "case 03" case03
    , testCase "case 04" case04
    , testCase "case 05" case05
    , testCase "case 06" case06
    , testCase "case 07" case07
    , testCase "case 08" case08
    , testCase "case 09" case09
    , testCase "case 10" case10
    , testCase "case 11" case11
    ]


--------------------------------------------------------------------------------
case01 :: Assertion
case01 = (@?= result) =<< format Nothing Nothing input
  where
    input = "module Herp where\ndata Foo = Bar | Baz { baz :: Int }"
    result = Right [ "module Herp where"
                   , "data Foo = Bar"
                   , "    | Baz"
                   , "    { baz :: Int"
                   , "    }"
                   ]


--------------------------------------------------------------------------------
case02 :: Assertion
case02 = withTestDirTree $ do
    writeFile "test-config.yaml" $ unlines
        [ "steps:"
        , "  - records: {}"
        , "indent: 2"
        ]

    actual <- format (Just $ ConfigPath "test-config.yaml") Nothing input
    actual @?= result
  where
    input = "module Herp where\ndata Foo = Bar | Baz { baz :: Int }"
    result = Right [ "module Herp where"
                   , "data Foo = Bar"
                   , "  | Baz"
                   , "  { baz :: Int"
                   , "  }"
                   ]


--------------------------------------------------------------------------------
case03 :: Assertion
case03 = (@?= result) =<< format Nothing (Just fileLocation) input
  where
    fileLocation = "directory/File.hs"
    input = "module Herp"
    result = Left $
      "Language.Haskell.Stylish.Parse.parseModule: could not parse " <>
      fileLocation <>
      ": ParseFailed (SrcLoc \"<unknown>.hs\" 2 1) \"Parse error: EOF\""


--------------------------------------------------------------------------------
-- | When providing current dir including folders and files.
case04 :: Assertion
case04 = withTestDirTree $ do
  createDirectory aDir >> writeFile c fileCont
  mapM_ (flip writeFile fileCont) fs
  result <- findHaskellFiles False input
  sort result @?= (sort $ map normalise expected)
  where
    input    = c : fs
    fs = ["b.hs", "a.hs"]
    c  = aDir </> "c.hs"
    aDir     = "aDir"
    expected = ["a.hs", "b.hs", c]
    fileCont = ""


--------------------------------------------------------------------------------
-- | When the input item is not file, do not recurse it.
case05 :: Assertion
case05 = withTestDirTree $ do
  mapM_ (flip writeFile "") input
  result <- findHaskellFiles False input
  result @?= expected
  where
    input    = ["b.hs"]
    expected = map normalise input


--------------------------------------------------------------------------------
-- | Empty input should result in empty output.
case06 :: Assertion
case06 = withTestDirTree $ do
  mapM_ (flip writeFile "") input
  result <- findHaskellFiles False input
  result @?= expected
  where
    input    = []
    expected = input


--------------------------------------------------------------------------------
-- | stylish-haskell -e "aDir/c.hs" "aDir/c.hs" a.hs b.hs
case07 :: Assertion
case07 = withTestDirTree $ do
  createDirectory aDir >> writeFile c fileCont
  mapM_ (flip writeFile fileCont) fs
  result <- withExceptions False (Just [aDir]) input
  sort result @?= (sort $ map normalise expected)
  where
    input    = c : fs
    expected = fs
    aDir     = "aDir"
    c        = aDir </> "c.hs"
    fs       = ["b.hs", "a.hs"]
    fileCont = ""


--------------------------------------------------------------------------------
-- | stylish-haskell -e "aDir/c.hs" "aDir/c.hs"
case08 :: Assertion
case08 = withTestDirTree $ do
  createDirectory aDir >> writeFile c fileCont
  result <- withExceptions False (Just [aDir]) input
  sort result @?= (sort $ map normalise expected)
  where
    input    = [c]
    expected = []
    aDir     = "aDir"
    c        = aDir </> "c.hs"
    fileCont = ""


--------------------------------------------------------------------------------
-- | stylish-haskell "aDir/c.hs"
case09 :: Assertion
case09 = withTestDirTree $ do
  createDirectory aDir >> writeFile c fileCont
  result <- withExceptions False Nothing input
  sort result @?= (sort $ map normalise expected)
  where
    input    = [c]
    expected = input
    aDir     = "aDir"
    c        = aDir </> "c.hs"
    fileCont = ""


--------------------------------------------------------------------------------
-- | stylish-haskell -e "aDir/c.hs" a.hs   ## However aDir/c.hs does not exists.
case10 :: Assertion
case10 = withTestDirTree $ do
  mapM_ (flip writeFile fileCont) fs
  result <- withExceptions False (Just [c]) input
  sort result @?= (sort $ map normalise expected)
  where
    input    = fs
    expected = fs
    aDir     = "aDir"
    c        = aDir </> "c.hs"
    fs       = ["a.hs"]
    fileCont = ""


--------------------------------------------------------------------------------
-- | stylish-haskell -e "aDir" a.hs ./aDir/c.hs
case11 :: Assertion
case11 = withTestDirTree $ do
  createDirectory aDir >> writeFile c fileCont
  mapM_ (flip writeFile fileCont) fs
  result <- withExceptions False (Just [aDir]) input
  sort result @?= (sort $ map normalise expected)
  where
    input    = c : fs
    expected = fs
    aDir     = "aDir"
    c        = aDir </> "c.hs"
    fs       = ["a.hs"]
    fileCont = ""
