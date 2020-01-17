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
    , testCase "case 03" case03
    , testCase "case 04" case04
    , testCase "case 05" case05
    , testCase "case 06" case06
    , testCase "case 07" case07
    , testCase "case 08" case08
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

case03 :: Assertion
case03 = expected @=? testStep step input
  where
    input = unlines
      [ "module Herp where"
      , ""
      , "data Foo a = Foo { a :: a, a2 :: String }"
      ]
    expected = unlines
       [ "module Herp where"
       , ""
       , "data Foo a = Foo"
       , "  { a :: a"
       , "  , a2 :: String"
       , "  }"
       ]

case04 :: Assertion
case04 = expected @=? testStep step input
  where
    input = unlines
      [ "module Herp where"
      , ""
      , "data Foo a = Foo { a :: a, a2 :: String } | Bar { b :: a }"
      ]
    expected = unlines
       [ "module Herp where"
       , ""
       , "data Foo a = Foo"
       , "  { a :: a"
       , "  , a2 :: String"
       , "  }"
       , "  | Bar"
       , "  { b :: a"
       , "  }"
       ]

case05 :: Assertion
case05 = expected @=? testStep step input
  where
    input = unlines
       [ "module Herp where"
       , ""
       , "data Foo = Foo {"
       , "  a :: Int"
       , "  , a2 :: String"
       , "  }"
       ]
    expected = unlines
       [ "module Herp where"
       , ""
       , "data Foo = Foo"
       , "  { a :: Int"
       , "  , a2 :: String"
       , "  }"
       ]

case06 :: Assertion
case06 = expected @=? testStep step input
  where
    input = unlines
      [ "module Herp where"
      , ""
      , "data Foo = Foo Int String"
      ]
    expected = input

case07 :: Assertion
case07 = expected @=? testStep step input
  where
    input = unlines
      [ "module Herp where"
      , ""
      , "data Phantom a = Phantom"
      ]
    expected = input

case08 :: Assertion
case08 = expected @=? testStep step input
  where
    input = unlines
      [ "module Herp where"
      , ""
      , "data Phantom a ="
      , "  Phantom"
      ]
    expected = unlines
      [ "module Herp where"
      , ""
      , "data Phantom a = Phantom"
      ]
