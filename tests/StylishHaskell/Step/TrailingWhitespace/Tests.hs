--------------------------------------------------------------------------------
module StylishHaskell.Step.TrailingWhitespace.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                         (Test, testGroup)
import           Test.Framework.Providers.HUnit         (testCase)
import           Test.HUnit                             (Assertion, (@=?))


--------------------------------------------------------------------------------
import           StylishHaskell.Step.TrailingWhitespace
import           StylishHaskell.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "StylishHaskell.Step.TrailingWhitespace.Tests"
    [ testCase "case 01" case01
    ]


--------------------------------------------------------------------------------
case01 :: Assertion
case01 = expected @=? testStep step input
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
