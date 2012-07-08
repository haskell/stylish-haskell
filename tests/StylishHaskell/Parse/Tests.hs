--------------------------------------------------------------------------------
module StylishHaskell.Parse.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (assert)


--------------------------------------------------------------------------------
import           StylishHaskell.Parse


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "StylishHaskell.Parse"
    [ testBom
    , testExtraExtensions
    ]


--------------------------------------------------------------------------------
testBom :: Test
testBom = testCase "UTF-8 Byte Order Mark" $ assert $ isRight $
    parseModule [] Nothing input
  where
    input = unlines
        [ '\xfeff' : "foo :: Int"
        , "foo = 3"
        ]


--------------------------------------------------------------------------------
testExtraExtensions :: Test
testExtraExtensions = testCase "Extra extensions" $ assert $ isRight $
    parseModule ["TemplateHaskell"] Nothing "$(foo)"


--------------------------------------------------------------------------------
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False
