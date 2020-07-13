module Language.Haskell.Stylish.Printer.ModuleHeader.Tests
  ( tests
  ) where

--------------------------------------------------------------------------------
import Test.Framework                 (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit                     (Assertion, (@=?))
import GHC.Stack                      (HasCallStack, withFrozenCallStack)
import Prelude hiding                 (lines)


--------------------------------------------------------------------------------
import Language.Haskell.Stylish.Module
import Language.Haskell.Stylish.Config (defaultConfig')
import Language.Haskell.Stylish.Parse (parseModule)
import Language.Haskell.Stylish.Printer.ModuleHeader (printModuleHeader)



--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Language.Haskell.Stylish.Printer.ModuleHeader"
  [ testCase "Hello world" ex0
  , testCase "Empty exports list" ex1
  , testCase "Single exported variable" ex2
  , testCase "Multiple exported variables" ex3
  ]

--------------------------------------------------------------------------------
ex0 :: Assertion
ex0 = input `assertFormatted` output
  where
    input =
      [ "module Foo where"
      ]
    output =
      [ "module Foo"
      , "  where"
      ]

ex1 :: Assertion
ex1 = input `assertFormatted` output
  where
    input =
      [ "module Foo () where"
      ]
    output =
      [ "module Foo"
      , "  ("
      , "  ) where"
      ]

ex2 :: Assertion
ex2 = input `assertFormatted` output
  where
    input =
      [ "module Foo (tests) where"
      ]
    output =
      [ "module Foo"
      , "  ( tests"
      , "  ) where"
      ]

ex3 :: Assertion
ex3 = input `assertFormatted` output
  where
    input =
      [ "module Foo (t1, t2, t3) where"
      ]
    output =
      [ "module Foo"
      , "  ( t1"
      , "  , t2"
      , "  , t3"
      , "  ) where"
      ]

--------------------------------------------------------------------------------
assertFormatted :: HasCallStack => Lines -> Lines -> Assertion
assertFormatted input expected = withFrozenCallStack $ expected @=? parseAndFormat input
  where
    parseAndFormat lines =
      case parseModule [] Nothing (unlines lines) of
        Right parsedModule ->
          printModuleHeader defaultConfig' (moduleComments parsedModule) (moduleHeader parsedModule)
        Left err ->
          error $ "parseAndFormat: Should've been able to parse input - " <> err
