--------------------------------------------------------------------------------
module StylishHaskell.Stylish.UnicodeSyntax.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     ((@=?))


--------------------------------------------------------------------------------
import           StylishHaskell.Stylish.UnicodeSyntax
import           StylishHaskell.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "StylishHaskell.Stylish.UnicodeSyntax.Tests"
    [ case01
    ]


--------------------------------------------------------------------------------
case01 :: Test
case01 = testCase "case 01" $ expected @=? testStylish stylish input
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
