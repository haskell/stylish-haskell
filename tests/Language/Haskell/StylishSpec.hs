module Language.Haskell.StylishSpec where

--------------------------------------------------------------------------------
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, (@?=))

--------------------------------------------------------------------------------
import           Language.Haskell.Stylish

--------------------------------------------------------------------------------
import           System.IO.Unsafe
--------------------------------------------------------------------------------

tests :: Test
tests = testGroup "Language.Haskell.Stylish.Step.Tabs.Tests"
    [ testCase "case 01" case01
    , testCase "case 02" case02
    , testCase "case 03" case03
    ]


--------------------------------------------------------------------------------
case01 :: Assertion
case01 = (@?=) result (unsafePerformIO $ format Nothing Nothing input)
  where
    input = "module Herp where\n data Foo = Bar | Baz"
    result = Right [ "module Herp where"
                   , "data Foo = Bar"
                   , "    | Baz"
                   ]

case02 :: Assertion
case02 = (@?=) result (unsafePerformIO $ format (Just configLocation) Nothing input)
  where
    configLocation = ConfigPath "testdata/test-config.yaml"
    input = "module Herp where\n data Foo = Bar | Baz"
    result = Right [ "module Herp where"
                   , "data Foo = Bar"
                   , "  | Baz"
                   ]

case03 :: Assertion
case03 = (@?=) result (unsafePerformIO $ format Nothing (Just fileLocation) input)
  where
    fileLocation = "directory/File.hs"
    input = "module Herp"
    result = Left $
      "Language.Haskell.Stylish.Parse.parseModule: could not parse " <>
      fileLocation <>
      ": ParseFailed (SrcLoc \"<unknown>.hs\" 2 1) \"Parse error: EOF\""
