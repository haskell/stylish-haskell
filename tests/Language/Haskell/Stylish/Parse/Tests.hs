--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Parse.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, assert)


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Parse


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Language.Haskell.Stylish.Parse"
    [ testCase "UTF-8 Byte Order Mark" testBom
    , testCase "Extra extensions"      testExtraExtensions
    ]


--------------------------------------------------------------------------------
testBom :: Assertion
testBom = assert $ isRight $ parseModule [] Nothing input
  where
    input = unlines
        [ '\xfeff' : "foo :: Int"
        , "foo = 3"
        ]


--------------------------------------------------------------------------------
testExtraExtensions :: Assertion
testExtraExtensions = assert $ isRight $
    parseModule ["TemplateHaskell"] Nothing "$(foo)"


--------------------------------------------------------------------------------
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False
