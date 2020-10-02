module Language.Haskell.Stylish.Parse.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, assertFailure)
import           GHC.Stack                      (HasCallStack, withFrozenCallStack)


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
    , testCase "ShebangDouble"               testShebangDouble
    , testCase "GADTs extension"             testGADTs
    , testCase "KindSignatures extension"    testKindSignatures
    , testCase "StandalonDeriving extension" testStandaloneDeriving
    , testCase "UnicodeSyntax extension"     testUnicodeSyntax
    , testCase "XmlSyntax regression"        testXmlSyntaxRegression
    , testCase "MagicHash regression"        testMagicHashRegression
    ]

--------------------------------------------------------------------------------
testShebangExt :: Assertion
testShebangExt = returnsRight $ parseModule [] Nothing input
  where
    input = unlines
      [ "#!env runghc"
      , "{-# LANGUAGE CPP #-}"
      , "#define foo bar \\"
      , "             qux"
      ]

--------------------------------------------------------------------------------
testBom :: Assertion
testBom = returnsRight $ parseModule [] Nothing input
  where
    input = unlines
        [ '\xfeff' : "foo :: Int"
        , "foo = 3"
        ]


--------------------------------------------------------------------------------
testExtraExtensions :: Assertion
testExtraExtensions = returnsRight $
    parseModule ["TemplateHaskell"] Nothing "$(foo)"


--------------------------------------------------------------------------------
testMultilineCpp :: Assertion
testMultilineCpp = returnsRight $ parseModule [] Nothing $ unlines
    [ "{-# LANGUAGE CPP #-}"
    , "#define foo bar \\"
    , "             qux"
    ]


--------------------------------------------------------------------------------
testHaskell2010 :: Assertion
testHaskell2010 = returnsRight $ parseModule [] Nothing $ unlines
    [ "{-# LANGUAGE Haskell2010 #-}"
    , "module X where"
    , "foo x | Just y <- x = y"
    ]


--------------------------------------------------------------------------------
testShebang :: Assertion
testShebang = returnsRight $ parseModule [] Nothing $ unlines
    [ "#!runhaskell"
    , "module Main where"
    , "main = return ()"
    ]

--------------------------------------------------------------------------------

testShebangDouble :: Assertion
testShebangDouble = returnsRight $ parseModule [] Nothing $ unlines
    [ "#!nix-shell"
    , "#!nix-shell -i runhaskell -p haskellPackages.ghc"
    , "module Main where"
    , "main = return ()"
    ]

--------------------------------------------------------------------------------

-- | These tests are for syntactic language extensions that should always be
-- enabled for parsing, even when the pragma is absent.

testGADTs :: Assertion
testGADTs = returnsRight $ parseModule [] Nothing $ unlines
  [ "module Main where"
  , "data SafeList a b where"
  , "  Nil :: SafeList a Empty"
  , "  Cons:: a -> SafeList a b -> SafeList a NonEmpty"
  ]

testKindSignatures :: Assertion
testKindSignatures = returnsRight $ parseModule [] Nothing $ unlines
  [ "module Main where"
  , "data D :: * -> * -> * where"
  , "  D :: a -> b -> D a b"
  ]

testStandaloneDeriving :: Assertion
testStandaloneDeriving = returnsRight $ parseModule [] Nothing $ unlines
  [ "module Main where"
  , "deriving instance Show MyType"
  ]

testUnicodeSyntax :: Assertion
testUnicodeSyntax = returnsRight $ parseModule [] Nothing $ unlines
  [ "module Main where"
  , "monadic ∷ (Monad m) ⇒ m a → m a"
  , "monadic = id"
  ]

testXmlSyntaxRegression :: Assertion
testXmlSyntaxRegression = returnsRight $ parseModule [] Nothing $ unlines
  [ "smaller a b = a <b"
  ]

testMagicHashRegression :: Assertion
testMagicHashRegression = returnsRight $ parseModule [] Nothing $ unlines
  [ "xs = \"foo\"#|1#|'a'#|bar#|Nil"
  ]

--------------------------------------------------------------------------------
returnsRight :: HasCallStack => Show a => Either a b -> Assertion
returnsRight action = withFrozenCallStack $ either (assertFailure . show) mempty action
