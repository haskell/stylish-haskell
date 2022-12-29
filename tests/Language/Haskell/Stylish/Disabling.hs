{-# LANGUAGE OverloadedLists #-}
module Language.Haskell.Stylish.Disabling where

import qualified Language.Haskell.Stylish.Step.ModuleHeader  as Header
import qualified Language.Haskell.Stylish.Step.Data          as Data
import qualified Language.Haskell.Stylish.Step.UnicodeSyntax as Unicode
import           Language.Haskell.Stylish.Tests.Util (assertSnippet)
import           Test.Framework                      (Test, testGroup)
import           Test.Framework.Providers.HUnit      (testCase)
import           Test.HUnit                          (Assertion)


tests :: Test
tests = testGroup "Language.Haskell.Stylish.Disabling"
    [ testCase "Header formatiing disabled" case00
    , testCase "One of several Datas formatting disabled" case01
    , testCase "Unicode (one-symbol replacement)" case02
    ]

--------------------------------------------------------------------------------
case00 :: Assertion
case00 = assertSnippet (Header.step (Just 80) Header.defaultConfig) inp inp
  where
    inp =
      [ "{- STYLISH_DISABLE -}"
      , "module Main (foo, bar) where"
      , "{- STYLISH_ENABLE -}"
      ]

--------------------------------------------------------------------------------
case01 :: Assertion
case01 = assertSnippet (Data.step Data.defaultConfig)
  [ "data Foo = Foo"
  , ""
  , "{-     stylish_disable   -}"
  , "data Bar = Bar"
  , "{-     stylish_enable    -}"
  , "data Baz = Baz"
  ]
  [ "data Foo"
  , "    = Foo"
  , ""
  , "{-     stylish_disable   -}"
  , "data Bar = Bar"
  , "{-     stylish_enable    -}"
  , "data Baz"
  , "    = Baz"
  ]


--------------------------------------------------------------------------------
case02 :: Assertion
case02 = assertSnippet (Unicode.step True "LANGUAGE")
  [ "foo :: Int -> String"
  , "foo = undefined"
  , "{- stylish_disable -}"
  , "bar :: Int"
  , "bar = undefined"
  , "{- stylish_enable -}"
  , ""
  , "baz :: String {- stylish_disable -}"
  , "baz = undefined"
  , "{- stylish_enable -} baz' :: Int"
  , "baz' = undefined"
  ]
  [ "{-# LANGUAGE UnicodeSyntax #-}"
  , "foo ∷ Int → String"
  , "foo = undefined"
  , "{- stylish_disable -}"
  , "bar :: Int"
  , "bar = undefined"
  , "{- stylish_enable -}"
  , ""
  , "baz :: String {- stylish_disable -}"
  , "baz = undefined"
  , "{- stylish_enable -} baz' :: Int"
  , "baz' = undefined"
  ]
