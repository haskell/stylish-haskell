--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists #-}
module Language.Haskell.Stylish.Step.Tabs.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion)


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Step.Tabs
import           Language.Haskell.Stylish.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Language.Haskell.Stylish.Step.Tabs.Tests"
    [ testCase "case 01" case01
    ]


--------------------------------------------------------------------------------
case01 :: Assertion
case01 = assertSnippet (step 4)
    [ "module Main"
    , "\t\twhere"
    , "data Foo"
    , "\t= Bar"
    , "    | Qux"
    ]
    [ "module Main"
    , "        where"
    , "data Foo"
    , "    = Bar"
    , "    | Qux"
    ]
