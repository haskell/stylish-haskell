module Language.Haskell.Stylish.Step.Data.Tests
    ( tests
    ) where

import           Language.Haskell.Stylish.Step.Data
import           Language.Haskell.Stylish.Tests.Util (testStep)
import           Test.Framework                      (Test, testGroup)
import           Test.Framework.Providers.HUnit      (testCase)
import           Test.HUnit                          (Assertion, (@=?))

tests :: Test
tests = testGroup "Language.Haskell.Stylish.Step.Data.Tests"
    [ testCase "case 01" case01]

simpleInput :: String
simpleInput = unlines
  [ "module Herp where"
  , ""
  , "data Foo = Foo { a :: Int }"
  ]

case01 :: Assertion
case01 = expected @=? testStep step simpleInput
  where
    expected = unlines
       [ "module Herp where"
       , ""
       , "data Foo = Foo"
       , "  { a :: Int"
       , "  }"
       ]
