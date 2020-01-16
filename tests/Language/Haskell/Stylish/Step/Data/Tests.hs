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
    [ testCase "case 01" case01
    , testCase "case 02" case02
    ]

case01 :: Assertion
case01 = expected @=? testStep step input
  where
    input = unlines
      [ "module Herp where"
      , ""
      , "data Foo = Foo { a :: Int }"
      ]

    expected = unlines
       [ "module Herp where"
       , ""
       , "data Foo = Foo"
       , "  { a :: Int"
       , "  }"
       ]

case02 :: Assertion
case02 = expected @=? testStep step input
  where
    input = unlines
      [ "module Herp where"
      , ""
      , "data Foo = Foo { a :: Int, a2 :: String }"
      ]
    expected = unlines
       [ "module Herp where"
       , ""
       , "data Foo = Foo"
       , "  { a :: Int"
       , "  , a2 :: String"
       , "  }"
       ]
