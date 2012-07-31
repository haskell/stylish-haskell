--------------------------------------------------------------------------------
module StylishHaskell.Step.UnicodeSyntax.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                    (Test, testGroup)
import           Test.Framework.Providers.HUnit    (testCase)
import           Test.HUnit                        (Assertion, (@=?))


--------------------------------------------------------------------------------
import           StylishHaskell.Step.UnicodeSyntax
import           StylishHaskell.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "StylishHaskell.Step.UnicodeSyntax.Tests"
    [ testCase "case 01" case01
    ]


--------------------------------------------------------------------------------
case01 :: Assertion
case01 = expected @=? testStep (step True) input
  where
    input = unlines
        [ "sort :: Ord a => [a] -> [a]"
        , "sort _ = []"
        ]

    expected = unlines
        [ "{-# LANGUAGE UnicodeSyntax #-}"
        , "sort ∷ Ord a ⇒ [a] → [a]"
        , "sort _ = []"
        ]
