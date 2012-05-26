--------------------------------------------------------------------------------
module StylishHaskell.TrailingWhitespace.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     ((@=?))


--------------------------------------------------------------------------------
import           StylishHaskell.TrailingWhitespace
import           StylishHaskell.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "StylishHaskell.TrailingWhitespace.Tests"
    [ case01
    ]


--------------------------------------------------------------------------------
case01 :: Test
case01 = testCase "case 01" $ expected @=? testStylish stylish input
  where
    input = unlines
        [ "module Main where"
        , " "
        , "data Foo = Bar | Qux\t "
        ]

    expected = unlines
        [ "module Main where"
        , ""
        , "data Foo = Bar | Qux"
        ]
