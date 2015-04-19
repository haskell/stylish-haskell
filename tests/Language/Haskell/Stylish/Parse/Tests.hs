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
    , testCase "Multiline CPP"         testMultilineCpp
    , testCase "Haskell2010 extension" testHaskell2010
    , testCase "Shebang"               testShebang
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
testMultilineCpp :: Assertion
testMultilineCpp = assert $ isRight $ parseModule [] Nothing $ unlines
    [ "{-# LANGUAGE CPP #-}"
    , "#define foo bar \\"
    , "             qux"
    ]


--------------------------------------------------------------------------------
testHaskell2010 :: Assertion
testHaskell2010 = assert $ isRight $ parseModule [] Nothing $ unlines
    [ "{-# LANGUAGE Haskell2010 #-}"
    , "module X where"
    , "foo x | Just y <- x = y"
    ]


--------------------------------------------------------------------------------
testShebang :: Assertion
testShebang = assert $ isRight $ parseModule [] Nothing $ unlines
    [ "#!runhaskell"
    , "module Main where"
    , "main = return ()"
    ]


--------------------------------------------------------------------------------
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False
