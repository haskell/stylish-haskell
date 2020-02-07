--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
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
    ]


--------------------------------------------------------------------------------
case01 :: Assertion
case01 = (@?= result) =<< format Nothing Nothing input
  where
    input = "module Herp where\ndata Foo = Bar | Baz { baz :: Int }"
    result = Right $ lines input


--------------------------------------------------------------------------------
case02 :: Assertion
case02 = withTestDirTree $ do
    writeFile "test-config.yaml" $ unlines
        [ "steps:"
        , "  - records:"
        , "      equals: \"indent 2\""
        , "      first_field: \"indent 2\""
        , "      field_comment: 2"
        , "      deriving: 2"
        ]

    actual <- format (Just $ ConfigPath "test-config.yaml") Nothing input
    actual @?= result
  where
    input = "module Herp where\ndata Foo = Bar | Baz { baz :: Int }"
    result = Right [ "module Herp where"
                   , "data Foo"
                   , "  = Bar"
                   , "  | Baz"
                   , "      { baz :: Int"
                   , "      }"
                   ]

--------------------------------------------------------------------------------
case03 :: Assertion
case03 = withTestDirTree $ do
    writeFile "test-config.yaml" $ unlines
        [ "steps:"
        , "  - records:"
        , "      equals: \"same_line\""
        , "      first_field: \"same_line\""
        , "      field_comment: 2"
        , "      deriving: 2"
        ]

    actual <- format (Just $ ConfigPath "test-config.yaml") Nothing input
    actual @?= result
  where
    input = unlines [ "module Herp where"
                    , "data Foo"
                    , "  = Bar"
                    , "  | Baz"
                    , "      { baz :: Int"
                    , "      }"
                    ]
    result = Right [ "module Herp where"
                   , "data Foo = Bar"
                   , "         | Baz { baz :: Int"
                   , "               }"
                   ]

--------------------------------------------------------------------------------
case04 :: Assertion
case04 = (@?= result) =<< format Nothing (Just fileLocation) input
  where
    fileLocation = "directory/File.hs"
    input = "module Herp"
    result = Left $
      "Language.Haskell.Stylish.Parse.parseModule: could not parse " <>
      fileLocation <>
      ": ParseFailed (SrcLoc \"<unknown>.hs\" 2 1) \"Parse error: EOF\""
