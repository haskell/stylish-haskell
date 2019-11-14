--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Step.UnicodeSyntax.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                    (Test, testGroup)
import           Test.Framework.Providers.HUnit    (testCase)
import           Test.HUnit                        (Assertion, (@=?))


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Step.UnicodeSyntax
import           Language.Haskell.Stylish.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Language.Haskell.Stylish.Step.UnicodeSyntax.Tests"
    [ testCase "case 01" case01
    , testCase "case 02" case02
    ]


--------------------------------------------------------------------------------
case01 :: Assertion
case01 = expected @=? testStep (step False True) input
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


--------------------------------------------------------------------------------
case02 :: Assertion
case02 = expected @=? testStep (step True True) input
  where
    input = unlines
        [ "sort :: Ord a => [a] -> [a]"
        , "sort _ = []"
        ]

    expected = unlines
        [ "{-# language UnicodeSyntax #-}"
        , "sort ∷ Ord a ⇒ [a] → [a]"
        , "sort _ = []"
        ]