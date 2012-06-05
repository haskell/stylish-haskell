--------------------------------------------------------------------------------
module StylishHaskell.Step.UnicodeSyntax.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     ((@=?))


--------------------------------------------------------------------------------
import           StylishHaskell.Step.UnicodeSyntax
import           StylishHaskell.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "StylishHaskell.Step.UnicodeSyntax.Tests"
    [ case01
    ]


--------------------------------------------------------------------------------
case01 :: Test
case01 = testCase "case 01" $ expected @=? testStep step input
  where
    input = unlines
        [ "{-# LANGUAGE UnicodeSyntax #-}"  -- TODO: remove
        , ""
        , "sort :: Ord a => [a] -> [a]"
        , "sort _ = []"
        ]

    expected = unlines
        [ "{-# LANGUAGE UnicodeSyntax #-}"
        , ""
        , "sort ∷ Ord a ⇒ [a] → [a]"
        , "sort _ = []"
        ]
