--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists #-}
module Language.Haskell.Stylish.Step.TrailingWhitespace.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                         (Test, testGroup)
import           Test.Framework.Providers.HUnit         (testCase)
import           Test.HUnit                             (Assertion)


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Step.TrailingWhitespace
import           Language.Haskell.Stylish.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Language.Haskell.Stylish.Step.TrailingWhitespace.Tests"
    [ testCase "case 01" case01
    ]


--------------------------------------------------------------------------------
case01 :: Assertion
case01 = assertSnippet step
    [ "module Main where"
    , " \t"
    , "data Foo = Bar | Qux\t "
    , "\12"    -- page break
    , "   \12" -- malformed page break
    ]
    [ "module Main where"
    , ""
    , "data Foo = Bar | Qux"
    , "\12" -- page break
    , ""
    ]
