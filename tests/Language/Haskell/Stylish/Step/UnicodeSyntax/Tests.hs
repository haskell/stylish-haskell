--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists #-}
module Language.Haskell.Stylish.Step.UnicodeSyntax.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                              (Test, testGroup)
import           Test.Framework.Providers.HUnit              (testCase)
import           Test.HUnit                                  (Assertion)


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Step.UnicodeSyntax
import           Language.Haskell.Stylish.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Language.Haskell.Stylish.Step.UnicodeSyntax.Tests"
    [ testCase "case 01" case01
    , testCase "case 02" case02
    , testCase "case 03" case03
    ]


--------------------------------------------------------------------------------
case01 :: Assertion
case01 = assertSnippet (step True "LANGUAGE")
    [ "sort :: Ord a => [a] -> [a]"
    , "sort _ = []"
    ]
    [ "{-# LANGUAGE UnicodeSyntax #-}"
    , "sort ∷ Ord a ⇒ [a] → [a]"
    , "sort _ = []"
    ]


--------------------------------------------------------------------------------
case02 :: Assertion
case02 = assertSnippet (step True "LaNgUaGe")
    [ "sort :: Ord a => [a] -> [a]"
    , "sort _ = []"
    ]
    [ "{-# LaNgUaGe UnicodeSyntax #-}"
    , "sort ∷ Ord a ⇒ [a] → [a]"
    , "sort _ = []"
    ]


--------------------------------------------------------------------------------
case03 :: Assertion
case03 = assertSnippet (step False "LANGUAGE")
    [ "x :: Int -> Int -> Int"
    , "x = undefined"
    ]
    [ "x ∷ Int → Int → Int"
    , "x = undefined"
    ]
