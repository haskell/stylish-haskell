module Language.Haskell.Stylish.Step.Imports'.Tests
  ( tests
  ) where

--------------------------------------------------------------------------------
import           Test.Framework                         (Test, testGroup)
import           Test.Framework.Providers.HUnit         (testCase)
import           Test.HUnit                             (Assertion, (@=?))
import           GHC.Stack                              (HasCallStack, withFrozenCallStack)
import           Prelude                                hiding (lines)


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Module
import           Language.Haskell.Stylish.Step.Imports' (step)
import           Language.Haskell.Stylish.Tests.Util    (testStep')
import qualified Language.Haskell.Stylish.Step.Imports' as Imports



--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Language.Haskell.Stylish.Printer.Imports"
  [ testCase "Hello world" ex0
  , testCase "Sorted simple" ex1
  , testCase "Sorted import lists" ex2
  , testCase "Sorted import lists and import decls" ex3
  , testCase "Import constructor all" ex4
  , testCase "Import constructor specific" ex5
  , testCase "Import constructor specific sorted" ex6
  ]

--------------------------------------------------------------------------------
ex0 :: Assertion
ex0 = input `assertFormatted` output
  where
    input =
      [ "import B"
      , "import A"
      ]
    output =
      [ "import A"
      , "import B"
      ]

ex1 :: Assertion
ex1 = input `assertFormatted` output
  where
    input =
      [ "import B"
      , "import A"
      , "import C"
      , "import A qualified"
      , "import B qualified as X"
      ]
    output =
      [ "import A"
      , "import A qualified"
      , "import B"
      , "import B qualified as X"
      , "import C"
      ]

ex2 :: Assertion
ex2 = input `assertFormatted` output
  where
    input =
      [ "import B"
      , "import A (X)"
      , "import C"
      , "import A qualified as Y (Y)"
      ]
    output =
      [ "import A (X)"
      , "import A qualified as Y (Y)"
      , "import B"
      , "import C"
      ]

ex3 :: Assertion
ex3 = input `assertFormatted` output
  where
    input =
      [ "import B"
      , "import A (X, Z, Y)"
      , "import C"
      , "import qualified A as A0 (b, Y, a)"
      , "import D qualified as D0 (Y, b, a)"
      , "import E qualified as E0 (b, a, Y)"
      ]
    output =
      [ "import A (X, Y, Z)"
      , "import A qualified as A0 (Y, a, b)"
      , "import B"
      , "import C"
      , "import D qualified as D0 (Y, a, b)"
      , "import E qualified as E0 (Y, a, b)"
      ]

ex4 :: Assertion
ex4 = input `assertFormatted` output
  where
    input =
      [ "import A (X, Z(..), Y)"
      ]
    output =
      [ "import A (X, Y, Z (..))"
      ]

ex5 :: Assertion
ex5 = input `assertFormatted` output
  where
    input =
      [ "import A (X, Z(Z), Y)"
      ]
    output =
      [ "import A (X, Y, Z (Z))"
      ]

ex6 :: Assertion
ex6 = input `assertFormatted` output
  where
    input =
      [ "import A (X, Z(X, Z, Y), Y)"
      ]
    output =
      [ "import A (X, Y, Z (X, Y, Z))"
      ]

--------------------------------------------------------------------------------
assertFormatted :: HasCallStack => Lines -> Lines -> Assertion
assertFormatted input expected = withFrozenCallStack $ expected @=? testStep' (step Imports.Config) input
