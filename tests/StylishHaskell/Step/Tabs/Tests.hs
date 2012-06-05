--------------------------------------------------------------------------------
module StylishHaskell.Step.Tabs.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     ((@=?))


--------------------------------------------------------------------------------
import           StylishHaskell.Step.Tabs
import           StylishHaskell.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "StylishHaskell.Step.Tabs.Tests" [case01]


--------------------------------------------------------------------------------
case01 :: Test
case01 = testCase "case 01" $ expected @=? testStep (step 4) input
  where
    input = unlines
        [ "module Main"
        , "\t\twhere"
        , "data Foo"
        , "\t= Bar"
        , "    | Qux"
        ]

    expected = unlines
        [ "module Main"
        , "        where"
        , "data Foo"
        , "    = Bar"
        , "    | Qux"
        ]

