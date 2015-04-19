--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Step.Records.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, (@=?))


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Step.Records
import           Language.Haskell.Stylish.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Language.Haskell.Stylish.Step.Records.Tests"
    [ testCase "case 01" case01
    , testCase "case 02" case02
    ]


--------------------------------------------------------------------------------
case01 :: Assertion
case01 = expected @=? testStep step input
  where
    input = unlines
        [ "data Foo = Foo"
        , "    { foo :: Int"
        , "    , barqux :: String"
        , "    } deriving (Show)"
        ]

    expected = unlines
        [ "data Foo = Foo"
        , "    { foo    :: Int"
        , "    , barqux :: String"
        , "    } deriving (Show)"
        ]


--------------------------------------------------------------------------------
case02 :: Assertion
case02 = input @=? testStep step input
  where
    -- Don't attempt to align this since a field spans multiple lines
    input = unlines
        [ "data Foo = Foo"
        , "    { foo :: Int"
        , "    , barqux"
        , "         :: String"
        , "    } deriving (Show)"
        ]
