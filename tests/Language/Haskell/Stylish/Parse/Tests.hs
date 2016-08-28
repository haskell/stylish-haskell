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
    [ testCase "UTF-8 Byte Order Mark"       testBom
    , testCase "Extra extensions"            testExtraExtensions
    , testCase "Multiline CPP"               testMultilineCpp
    , testCase "Haskell2010 extension"       testHaskell2010
    , testCase "Shebang"                     testShebang
    , testCase "ShebangExt"                  testShebangExt
    , testCase "GADTs extension"             testGADTs
    , testCase "KindSignatures extension"    testKindSignatures
    , testCase "StandalonDeriving extension" testStandaloneDeriving
    , testCase "UnicodeSyntax extension"     testUnicodeSyntax
    , testCase "XmlSyntax regression"        testXmlSyntaxRegression
    ]

--------------------------------------------------------------------------------
testShebangExt :: Assertion
testShebangExt = assert $ isRight $ parseModule [] Nothing input
                         where
                           input = unlines
                               [ "#!env runghc"
                               , "{-# LANGUAGE CPP #-}"
                               , "#define foo bar \\"
                               , "             qux"
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

-- | These tests are for syntactic language extensions that should always be
-- enabled for parsing, even when the pragma is absent.

testGADTs :: Assertion
testGADTs = assert $ isRight $ parseModule [] Nothing $ unlines
  [ "module Main where"
  , "data SafeList a b where"
  , "  Nil :: SafeList a Empty"
  , "  Cons:: a -> SafeList a b -> SafeList a NonEmpty"
  ]

testKindSignatures :: Assertion
testKindSignatures = assert $ isRight $ parseModule [] Nothing $ unlines
  [ "module Main where"
  , "data D :: * -> * -> * where"
  , "  D :: a -> b -> D a b"
  ]

testStandaloneDeriving :: Assertion
testStandaloneDeriving = assert $ isRight $ parseModule [] Nothing $ unlines
  [ "module Main where"
  , "deriving instance Show MyType"
  ]

testUnicodeSyntax :: Assertion
testUnicodeSyntax = assert $ isRight $ parseModule [] Nothing $ unlines
  [ "module Main where"
  , "monadic ∷ (Monad m) ⇒ m a → m a"
  , "monadic = id"
  ]

testXmlSyntaxRegression :: Assertion
testXmlSyntaxRegression = assert $ isRight $ parseModule [] Nothing $ unlines
  [ "smaller a b = a <b"
  ]

--------------------------------------------------------------------------------
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False
