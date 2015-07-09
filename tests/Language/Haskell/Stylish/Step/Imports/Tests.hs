--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Step.Imports.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, (@=?))


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Step.Imports
import           Language.Haskell.Stylish.Tests.Util


--------------------------------------------------------------------------------
defaultAlign :: Align
defaultAlign = Align Global AfterAlias Inline 4


--------------------------------------------------------------------------------
fromImportAlign :: ImportAlign -> Align
fromImportAlign align = defaultAlign { importAlign = align }

--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Language.Haskell.Stylish.Step.Imports.Tests"
    [ testCase "case 01" case01
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
    , testCase "case 13" case13
    , testCase "case 14" case14
    ]


--------------------------------------------------------------------------------
input :: String
input = unlines
    [ "module Herp where"
    , ""
    , "import qualified Data.Map  as M"
    , "import Control.Monad"
    , "import  Only.Instances()"
    , "import       Data.Map     (lookup, (!), insert, Map)"
    , "import Data.List as List (concat, foldl, foldr, head, init, last,\
     \ length, map, null, reverse, tail, (++))"
    , ""
    , "import Herp.Derp.Internals hiding (foo)"
    , "import  Foo (Bar (..))"
    , ""
    , "herp = putStrLn \"import Hello world\""
    ]


--------------------------------------------------------------------------------
case01 :: Assertion
case01 = expected @=? testStep (step 80 $ fromImportAlign Global) input
  where
    expected = unlines
        [ "module Herp where"
        , ""
        , "import           Control.Monad"
        , "import           Data.List           as List (concat, foldl, foldr, head, init,"
        , "                                              last, length, map, null, reverse,"
        , "                                              tail, (++))"
        , "import           Data.Map            (Map, insert, lookup, (!))"
        , "import qualified Data.Map            as M"
        , "import           Only.Instances      ()"
        , ""
        , "import           Foo                 (Bar (..))"
        , "import           Herp.Derp.Internals hiding (foo)"
        , ""
        , "herp = putStrLn \"import Hello world\""
        ]


--------------------------------------------------------------------------------
case02 :: Assertion
case02 = expected @=? testStep (step 80 $ fromImportAlign Group) input
  where
    expected = unlines
        [ "module Herp where"
        , ""
        , "import           Control.Monad"
        , "import           Data.List      as List (concat, foldl, foldr, head, init, last,"
        , "                                         length, map, null, reverse, tail, (++))"
        , "import           Data.Map       (Map, insert, lookup, (!))"
        , "import qualified Data.Map       as M"
        , "import           Only.Instances ()"
        , ""
        , "import Foo                 (Bar (..))"
        , "import Herp.Derp.Internals hiding (foo)"
        , ""
        , "herp = putStrLn \"import Hello world\""
        ]


--------------------------------------------------------------------------------
case03 :: Assertion
case03 = expected @=? testStep (step 80 $ fromImportAlign None) input
  where
    expected = unlines
        [ "module Herp where"
        , ""
        , "import Control.Monad"
        , "import Data.List as List (concat, foldl, foldr, head, init, last, length, map,"
        , "                          null, reverse, tail, (++))"
        , "import Data.Map (Map, insert, lookup, (!))"
        , "import qualified Data.Map as M"
        , "import Only.Instances ()"
        , ""
        , "import Foo (Bar (..))"
        , "import Herp.Derp.Internals hiding (foo)"
        , ""
        , "herp = putStrLn \"import Hello world\""
        ]


--------------------------------------------------------------------------------
case04 :: Assertion
case04 = expected @=? testStep (step 80 $ fromImportAlign Global) input'
  where
    input' =
        "import Data.Aeson.Types (object, typeMismatch, FromJSON(..)," ++
        "ToJSON(..), Value(..), parseEither, (.!=), (.:), (.:?), (.=))"

    expected = unlines
        [ "import           Data.Aeson.Types (FromJSON (..), ToJSON (..), Value (..),"
        , "                                   object, parseEither, typeMismatch, (.!=),"
        , "                                   (.:), (.:?), (.=))"
        ]


--------------------------------------------------------------------------------
case05 :: Assertion
case05 = input' @=? testStep (step 80 $ fromImportAlign Group) input'
  where
    input' = "import Distribution.PackageDescription.Configuration " ++
        "(finalizePackageDescription)\n"


--------------------------------------------------------------------------------
case06 :: Assertion
case06 = input' @=? testStep (step 80 $ fromImportAlign File) input'
  where
    input' = unlines
        [ "import Bar.Qux"
        , "import Foo.Bar"
        ]


--------------------------------------------------------------------------------
case07 :: Assertion
case07 = expected @=? testStep (step 80 $ fromImportAlign File) input'
  where
    input' = unlines
        [ "import Bar.Qux"
        , ""
        , "import qualified Foo.Bar"
        ]

    expected = unlines
        [ "import           Bar.Qux"
        , ""
        , "import qualified Foo.Bar"
        ]


--------------------------------------------------------------------------------
case08 :: Assertion
case08 = expected @=? testStep (step 80 $ Align Global WithAlias Inline 4) input
  where
    expected = unlines
        [ "module Herp where"
        , ""
        , "import           Control.Monad"
        , "import           Data.List           as List (concat, foldl, foldr, head, init,"
        , "                                     last, length, map, null, reverse, tail,"
        , "                                     (++))"
        , "import           Data.Map            (Map, insert, lookup, (!))"
        , "import qualified Data.Map            as M"
        , "import           Only.Instances      ()"
        , ""
        , "import           Foo                 (Bar (..))"
        , "import           Herp.Derp.Internals hiding (foo)"
        , ""
        , "herp = putStrLn \"import Hello world\""
        ]


--------------------------------------------------------------------------------
case09 :: Assertion
case09 = expected @=? testStep (step 80 $ Align Global WithAlias Multiline 4) input
  where
    expected = unlines
        [ "module Herp where"
        , ""
        , "import           Control.Monad"
        , "import           Data.List           as List"
        , "    ( concat"
        , "    , foldl"
        , "    , foldr"
        , "    , head"
        , "    , init"
        , "    , last"
        , "    , length"
        , "    , map"
        , "    , null"
        , "    , reverse"
        , "    , tail"
        , "    , (++)"
        , "    )"
        , "import           Data.Map            (Map, insert, lookup, (!))"
        , "import qualified Data.Map            as M"
        , "import           Only.Instances      ()"
        , ""
        , "import           Foo                 (Bar (..))"
        , "import           Herp.Derp.Internals hiding (foo)"
        , ""
        , "herp = putStrLn \"import Hello world\""
        ]


--------------------------------------------------------------------------------
case10 :: Assertion
case10 = expected @=? testStep (step 40 $ Align Group WithAlias Multiline 4) input
  where
    expected = unlines
        [ "module Herp where"
        , ""
        , "import           Control.Monad"
        , "import           Data.List      as List"
        , "    ( concat"
        , "    , foldl"
        , "    , foldr"
        , "    , head"
        , "    , init"
        , "    , last"
        , "    , length"
        , "    , map"
        , "    , null"
        , "    , reverse"
        , "    , tail"
        , "    , (++)"
        , "    )"
        , "import           Data.Map"
        , "    ( Map"
        , "    , insert"
        , "    , lookup"
        , "    , (!)"
        , "    )"
        , "import qualified Data.Map       as M"
        , "import           Only.Instances ()"
        , ""
        , "import Foo                 (Bar (..))"
        , "import Herp.Derp.Internals hiding (foo)"
        , ""
        , "herp = putStrLn \"import Hello world\""
        ]


--------------------------------------------------------------------------------
case11 :: Assertion
case11 = expected @=? testStep (step 80 $ Align Group NewLine Inline 4) input
  where
    expected = unlines
        [ "module Herp where"
        , ""
        , "import           Control.Monad"
        , "import           Data.List      as List"
        , "    (concat, foldl, foldr, head, init, last, length, map, null, reverse, tail,"
        , "    (++))"
        , "import           Data.Map"
        , "    (Map, insert, lookup, (!))"
        , "import qualified Data.Map       as M"
        , "import           Only.Instances"
        , "    ()"
        , ""
        , "import Foo"
        , "    (Bar (..))"
        , "import Herp.Derp.Internals hiding"
        , "    (foo)"

        , ""
        , "herp = putStrLn \"import Hello world\""
        ]


--------------------------------------------------------------------------------
case12 :: Assertion
case12 = expected @=? testStep (step 80 $ Align Group NewLine Inline 2) input'
  where
    input' = unlines
        [ "import Data.List (map)"
        ]

    expected = unlines
        [ "import Data.List"
        , "  (map)"
        ]


--------------------------------------------------------------------------------
case13 :: Assertion
case13 = expected
    @=? testStep (step 80 $ Align None WithAlias InlineWithBreak 4) input'
  where
    input' = unlines
        [ "import qualified Data.List as List (concat, foldl, foldr, head, init,"
        , "    last, length, map, null, reverse, tail, (++))"
        ]

    expected = unlines
        [ "import qualified Data.List as List"
        , "    (concat, foldl, foldr, head, init, last, length, map, null, reverse, tail,"
        , "    (++))"
        ]


--------------------------------------------------------------------------------
case14 :: Assertion
case14 = expected
    @=? testStep (step 80 $ Align None WithAlias InlineWithBreak 10) expected
  where
    expected = unlines
        [ "import qualified Data.List as List (concat, map, null, reverse, tail, (++))"
        ]
