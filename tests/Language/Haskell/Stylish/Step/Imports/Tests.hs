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
defaultAlign = Align Global AfterAlias Inline Inherit 4 True


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
    , testCase "case 15" case15
    , testCase "case 16" case16
    , testCase "case 17" case17
    , testCase "case 18" case18
    , testCase "case 19" case19
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
case08 = expected
    @=? testStep (step 80 $ Align Global WithAlias Inline Inherit 4 True) input
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
case09 = expected
    @=? testStep (step 80 $ Align Global WithAlias Multiline Inherit 4 True) input
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
case10 = expected
    @=? testStep (step 40 $ Align Group WithAlias Multiline Inherit 4 True) input
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
case11 = expected
    @=? testStep (step 80 $ Align Group NewLine Inline Inherit 4 True) input
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
case12 = expected
    @=? testStep (step 80 $ Align Group NewLine Inline Inherit 2 True) input'
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
    @=? testStep (step 80 $ Align None WithAlias InlineWithBreak Inherit 4 True) input'
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
    @=? testStep
      (step 80 $ Align None WithAlias InlineWithBreak Inherit 10 True) expected
  where
    expected = unlines
        [ "import qualified Data.List as List (concat, map, null, reverse, tail, (++))"
        ]


--------------------------------------------------------------------------------
case15 :: Assertion
case15 = expected
    @=? testStep (step 80 $ Align None AfterAlias Multiline Inherit 4 True) input'
  where
    expected = unlines
        [ "import Data.Acid (AcidState)"
        , "import qualified Data.Acid as Acid"
        , "    ( closeAcidState"
        , "    , createCheckpoint"
        , "    , openLocalStateFrom"
        , "    )"
        , "import Data.Default.Class (Default (def))"
        , ""
        , "import qualified Herp.Derp.Internal.Types.Foobar as Internal (bar, foo)"
        ]

    input' = unlines
        [ "import Data.Acid (AcidState)"
        , "import qualified Data.Acid as Acid (closeAcidState, createCheckpoint, openLocalStateFrom)"
        , "import Data.Default.Class (Default (def))"
        , ""
        , "import qualified Herp.Derp.Internal.Types.Foobar as Internal (foo, bar)"
        ]


--------------------------------------------------------------------------------
case16 :: Assertion
case16 = expected
    @=? testStep (step 80 $ Align None AfterAlias Multiline Inherit 4 False) input'
  where
    expected = unlines
        [ "import Data.Acid (AcidState)"
        , "import Data.Default.Class (Default(def))"
        , ""
        , "import Data.Maybe (Maybe(Just, Nothing))"
        , ""
        , "import Data.Foo (Foo(Bar, Foo), Goo(Goo))"
        ]

    input' = unlines
        [ "import Data.Acid (AcidState)"
        , "import Data.Default.Class (Default(def))"
        , ""
        , "import Data.Maybe (Maybe   (Just, Nothing))"
        , ""
        , "import Data.Foo (Foo (Foo,Bar), Goo(Goo))"
        ]


--------------------------------------------------------------------------------
case17 :: Assertion
case17 = expected
    @=? testStep (step 80 $ Align None AfterAlias Multiline Inherit 4 True) input'
  where
    expected = unlines
        [ "import Control.Applicative (Applicative (pure, (<*>)))"
        , ""
        , "import Data.Identity (Identity (Identity, runIdentity))"
        ]

    input' = unlines
        [ "import Control.Applicative (Applicative ((<*>),pure))"
        , ""
        , "import Data.Identity (Identity (runIdentity,Identity))"
        ]


--------------------------------------------------------------------------------
case18 :: Assertion
case18 = expected @=? testStep
    (step 40 $ Align None AfterAlias InlineToMultiline Inherit 4 True) input'
  where
    expected = unlines
           ----------------------------------------
        [ "import Data.Foo as Foo (Bar, Baz, Foo)"
        , ""
        , "import Data.Identity"
        , "    (Identity (Identity, runIdentity))"
        , ""
        , "import Data.Acid as Acid"
        , "    ( closeAcidState"
        , "    , createCheckpoint"
        , "    , openLocalStateFrom"
        , "    )"
        ]

    input' = unlines
        [ "import Data.Foo as Foo (Bar, Baz, Foo)"
        , ""
        , "import Data.Identity (Identity (Identity, runIdentity))"
        , ""
        , "import Data.Acid as Acid (closeAcidState, createCheckpoint, openLocalStateFrom)"
        ]

--------------------------------------------------------------------------------
case19 :: Assertion
case19 = expected @=? testStep
    (step 40 $ Align Global NewLine InlineWithBreak RightAfter 17 True) input'
  where
    expected = unlines
           ----------------------------------------
        [ "import           Prelude ()"
        , "import           Prelude.Compat hiding"
        , "                 (foldMap)"
        , ""
        , "import           Data.List"
        , "                 (foldl', intercalate,"
        , "                 intersperse)"
        ]

    input' = unlines
        [ "import Prelude.Compat hiding (foldMap)"
        , "import Prelude ()"
        , ""
        , "import Data.List (foldl', intercalate, intersperse)"
        ]
