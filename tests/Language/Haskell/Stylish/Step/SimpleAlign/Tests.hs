--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Step.SimpleAlign.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                        (Test, testGroup)
import           Test.Framework.Providers.HUnit        (testCase)
import           Test.HUnit                            (Assertion, (@=?))


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Step.SimpleAlign
import           Language.Haskell.Stylish.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Language.Haskell.Stylish.Step.SimpleAlign.Tests"
    [ testCase "case 01" case01
    , testCase "case 02" case02
    , testCase "case 03" case03
    ]


--------------------------------------------------------------------------------
case01 :: Assertion
case01 = expected @=? testStep (step 80 defaultConfig) input
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


--------------------------------------------------------------------------------
case02 :: Assertion
case02 = expected @=? testStep (step 80 defaultConfig) input
  where
    input = unlines
        [ "eitherToMaybe (Left _) = Nothing"
        , "eitherToMaybe (Right x) = Just x"
        ]

    expected = unlines
        [ "eitherToMaybe (Left _)  = Nothing"
        , "eitherToMaybe (Right x) = Just x"
        ]


--------------------------------------------------------------------------------
case03 :: Assertion
case03 = expected @=? testStep (step 80 defaultConfig) input
  where
    input = unlines
        [ "heady def [] = def"
        , "heady _ (x : _) = x"
        ]

    expected = unlines
        [ "heady def []    = def"
        , "heady _ (x : _) = x"
        ]
