--------------------------------------------------------------------------------
module StylishHaskell.Step.Records.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, (@=?))


--------------------------------------------------------------------------------
import           StylishHaskell.Step.Records
import           StylishHaskell.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "StylishHaskell.Step.Records.Tests"
    [ testCase "case 01" case01
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
