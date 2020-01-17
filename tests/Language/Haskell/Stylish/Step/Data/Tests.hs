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
    [ testCase "case 00" case00
    , testCase "case 01" case01
    , testCase "case 02" case02
    , testCase "case 03" case03
    , testCase "case 04" case04
    , testCase "case 05" case05
    , testCase "case 06" case06
    , testCase "case 07" case07
    , testCase "case 08" case08
    , testCase "case 09" case09
    , testCase "case 10" case10
    , testCase "case 11" case11
    , testCase "case 12" case12
    ]

case00 :: Assertion
case00 = expected @=? testStep (step 2) input
  where
    input = unlines
      [ "module Herp where"
      , ""
      , "data Foo"
      ]

    expected = input

case01 :: Assertion
case01 = expected @=? testStep (step 2) input
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
case02 = expected @=? testStep (step 2) input
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
case03 = expected @=? testStep (step 2) input
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
case04 = expected @=? testStep (step 2) input
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
case05 = expected @=? testStep (step 2) input
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
case06 = expected @=? testStep (step 2) input
  where
    input = unlines
      [ "module Herp where"
      , ""
      , "data Foo = Foo Int String"
      ]
    expected = input

case07 :: Assertion
case07 = expected @=? testStep (step 2) input
  where
    input = unlines
      [ "module Herp where"
      , ""
      , "data Phantom a = Phantom"
      ]
    expected = input

case08 :: Assertion
case08 = expected @=? testStep (step 2) input
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

case09 :: Assertion
case09 = expected @=? testStep (step 4) input
  where
    input = unlines
      [ "module Herp where"
      , ""
      , "data Foo a b = Foo { a :: a, a2 :: String } | Bar { b :: a, c:: b }"
      ]
    expected = unlines
       [ "module Herp where"
       , ""
       , "data Foo a b = Foo"
       , "    { a :: a"
       , "    , a2 :: String"
       , "    }"
       , "    | Bar"
       , "    { b :: a"
       , "    , c :: b"
       , "    }"
       ]

case10 :: Assertion
case10 = expected @=? testStep (step 2) input
  where
    input = unlines
      [ "module Herp where"
      , ""
      , "data Foo = Foo { a :: Int } deriving (Eq, Generic) deriving (Show)"
      ]

    expected = unlines
       [ "module Herp where"
       , ""
       , "data Foo = Foo"
       , "  { a :: Int"
       , "  }"
       , "  deriving (Eq, Generic)"
       , "  deriving (Show)"
       ]

case11 :: Assertion
case11 = expected @=? testStep (step 2) input
  where
    input = unlines
      [ "{-# LANGUAGE DerivingStrategies #-}"
      , "module Herp where"
      , ""
      , "data Foo = Foo { a :: Int } deriving stock (Show)"
      ]

    expected = unlines
       [ "{-# LANGUAGE DerivingStrategies #-}"
       , "module Herp where"
       , ""
       , "data Foo = Foo"
       , "  { a :: Int"
       , "  }"
       , "  deriving stock (Show)"
       ]

case12 :: Assertion
case12 = expected @=? testStep (step 4) input
  where
    input = unlines
      [ "module Herp where"
      , ""
      , "data Point = Point { pointX, pointY :: Double , pointName :: String} deriving (Show)"
      ]

    expected = unlines
       [ "module Herp where"
       , ""
       , "data Point = Point"
       , "    { pointX, pointY :: Double"
       , "    , pointName :: String"
       , "    }"
       , "    deriving (Show)"
       ]
