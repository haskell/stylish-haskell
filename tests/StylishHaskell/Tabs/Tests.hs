--------------------------------------------------------------------------------
module StylishHaskell.Tabs.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     ((@=?))


--------------------------------------------------------------------------------
import           StylishHaskell.Tabs
import           StylishHaskell.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "StylishHaskell.Tabs.Tests" [case01]


--------------------------------------------------------------------------------
case01 :: Test
case01 = testCase "case 01" $ expected @=? testStylish stylish input
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

