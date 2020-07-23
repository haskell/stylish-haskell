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
import           Language.Haskell.Stylish.Step.Imports (defaultOptions)
import           Language.Haskell.Stylish.Step.Imports' (step)
import           Language.Haskell.Stylish.Tests.Util    (testStep')



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
  , testCase "Imports step does not change rest of file" ex7
  , testCase "Imports respect groups" ex8
  , testCase "Imports respects whitespace between groups" ex9
  , testCase "Doesn't add extra space after 'hiding'" ex10
  , testCase "Should be able to format symbolic imports" ex11
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

ex7 :: Assertion
ex7 = input `assertFormatted` output
  where
    input =
      [ "module Foo (tests) where"
      , "import B"
      , "import A (X, Z, Y)"
      , "import C"
      , "import qualified A as A0 (b, Y, a)"
      , "import D qualified as D0 (Y, b, a)"
      , "import E qualified as E0 (b, a, Y)"
      , "-- hello"
      , "foo :: Int"
      , "foo = 1"
      ]
    output =
      [ "module Foo (tests) where"
      , "import A (X, Y, Z)"
      , "import A qualified as A0 (Y, a, b)"
      , "import B"
      , "import C"
      , "import D qualified as D0 (Y, a, b)"
      , "import E qualified as E0 (Y, a, b)"
      , "-- hello"
      , "foo :: Int"
      , "foo = 1"
      ]

ex8 :: Assertion
ex8 = input `assertFormatted` output
  where
    input =
      [ "import B"
      , "-- Group divisor"
      , "import A (X)"
      , "import C"
      , "import A qualified as Y (Y)"
      ]
    output =
      [ "import B"
      , "-- Group divisor"
      , "import A (X)"
      , "import A qualified as Y (Y)"
      , "import C"
      ]

ex9 :: Assertion
ex9 = input `assertFormatted` output
  where
    input =
      [ "--------"
      , "import B"
      , ""
      , "-- Group divisor"
      , "import A (X)"
      , "import C"
      , "import A qualified as Y (Y)"
      ]
    output =
      [ "--------"
      , "import B"
      , ""
      , "-- Group divisor"
      , "import A (X)"
      , "import A qualified as Y (Y)"
      , "import C"
      ]

ex10 :: Assertion
ex10 = input `assertFormatted` output
  where
    input =
      [ "import B         hiding      (X)"
      , "import A  hiding (X)"
      ]
    output =
      [ "import A hiding (X)"
      , "import B hiding (X)"
      ]

ex11 :: Assertion
ex11 = input `assertFormatted` output
  where
    input =
      [ "import Data.Aeson ((.=))"
      , "import A  hiding (X)"
      ]
    output =
      [ "import A hiding (X)"
      , "import Data.Aeson ((.=))"
      ]

--------------------------------------------------------------------------------
assertFormatted :: HasCallStack => Lines -> Lines -> Assertion
assertFormatted input expected = withFrozenCallStack $ expected @=? testStep' (step defaultOptions) input
