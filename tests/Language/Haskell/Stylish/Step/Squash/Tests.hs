--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists #-}
module Language.Haskell.Stylish.Step.Squash.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                       (Test, testGroup)
import           Test.Framework.Providers.HUnit       (testCase)
import           Test.HUnit                           (Assertion)


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Step.Squash
import           Language.Haskell.Stylish.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Language.Haskell.Stylish.Step.SimpleSquash.Tests"
    [ testCase "case 01" case01
    , testCase "case 02" case02
    , testCase "case 03" case03
    , testCase "case 04" case04
    , testCase "case 05" case05
    , testCase "case 06 (issue #355)" case06
    ]


--------------------------------------------------------------------------------
case01 :: Assertion
case01 = assertSnippet step
    [ "data Foo = Foo"
    , "    { foo    :: Int"
    , "    , barqux :: String"
    , "    } deriving (Show)"
    ]
    [ "data Foo = Foo"
    , "    { foo :: Int"
    , "    , barqux :: String"
    , "    } deriving (Show)"
    ]


--------------------------------------------------------------------------------
case02 :: Assertion
case02 = assertSnippet step
    [ "data Foo = Foo"
    , "    { fooqux"
    , "    , bar    :: String"
    , "    } deriving (Show)"
    ]
    [ "data Foo = Foo"
    , "    { fooqux"
    , "    , bar :: String"
    , "    } deriving (Show)"
    ]


--------------------------------------------------------------------------------
case03 :: Assertion
case03 = assertSnippet step
    [ "maybe y0 f mx ="
    , "    case mx of"
    , "        Nothing -> y0"
    , "        Just x  -> f x"
    ]
    [ "maybe y0 f mx ="
    , "    case mx of"
    , "        Nothing -> y0"
    , "        Just x -> f x"
    ]


--------------------------------------------------------------------------------
case04 :: Assertion
case04 = assertSnippet step
    [ "maybe y0 f mx ="
    , "    case mx of"
    , "        Nothing ->"
    , "            y0"
    , "        Just x  ->"
    , "            f x"
    ]
    [ "maybe y0 f mx ="
    , "    case mx of"
    , "        Nothing ->"
    , "            y0"
    , "        Just x ->"
    , "            f x"
    ]


--------------------------------------------------------------------------------
case05 :: Assertion
case05 = assertSnippet step
    [ "maybe y0 _ Nothing  = y"
    , "maybe _  f (Just x) = f x"
    ]
    [ "maybe y0 _ Nothing = y"
    , "maybe _  f (Just x) = f x"
    ]


--------------------------------------------------------------------------------
-- See <https://github.com/haskell/stylish-haskell/issues/355>
case06 :: Assertion
case06 = assertSnippet step
    [ "main  =  (\\x ->  putStrLn x) \"Hello, World!\""
    ]
    [ "main = (\\x -> putStrLn x) \"Hello, World!\""
    ]
