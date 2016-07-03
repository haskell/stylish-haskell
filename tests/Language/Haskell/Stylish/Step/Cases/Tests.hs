--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Step.Cases.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                        (Test, testGroup)
import           Test.Framework.Providers.HUnit        (testCase)
import           Test.HUnit                            (Assertion, (@=?))


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Step.Cases
import           Language.Haskell.Stylish.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Language.Haskell.Stylish.Step.Records.Tests"
    [ testCase "case 01" case01
    ]


--------------------------------------------------------------------------------
case01 :: Assertion
case01 = expected @=? testStep (step 80) input
  where
    input = unlines
        [ "eitherToMaybe e = case e of"
        , "    Left _ -> Nothing"
        , "    Right x -> Just x"
        ]

    expected = unlines
        [ "eitherToMaybe e = case e of"
        , "    Left _  -> Nothing"
        , "    Right x -> Just x"
        ]
