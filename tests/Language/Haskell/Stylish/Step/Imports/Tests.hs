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
fromImportAlign :: ImportAlign -> Options
fromImportAlign align = defaultOptions { importAlign = align }


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
    , testCase "case 08b" case08b
    , testCase "case 09" case09
    , testCase "case 10" case10
    , testCase "case 11" case11
    , testCase "case 11b" case11b
    , testCase "case 12" case12
    , testCase "case 12b" case12b
    , testCase "case 13" case13
    , testCase "case 13b" case13b
    , testCase "case 14" case14
    , testCase "case 15" case15
    , testCase "case 16" case16
    , testCase "case 17" case17
    , testCase "case 18" case18
    , testCase "case 19" case19
    , testCase "case 19b" case19b
    , testCase "case 19d" case19c
    , testCase "case 19d" case19d
    , testCase "case 20" case20
    , testCase "case 21" case21
    , testCase "case 22" case22
    , testCase "case 23" case23
    , testCase "case 23b" case23b
    , testCase "case 24" case24
    , testCase "case 25" case25
    , testCase "case 26 (issue 185)" case26
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
    @=? testStep (step 80 $ Options Global WithAlias True Inline Inherit (LPConstant 4) True False) input
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
case08b :: Assertion
case08b = expected
    @=? testStep (step 80 $ Options Global WithModuleName True Inline Inherit (LPConstant 4) True False) input
  where
    expected = unlines
        ["module Herp where"
        , ""
        , "import           Control.Monad"
        , "import           Data.List           as List (concat, foldl, foldr, head, init,"
        , "                     last, length, map, null, reverse, tail, (++))"
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
    @=? testStep (step 80 $ Options Global WithAlias True Multiline Inherit (LPConstant 4) True False) input
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
    @=? testStep (step 40 $ Options Group WithAlias True Multiline Inherit (LPConstant 4) True False) input
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
    @=? testStep (step 80 $ Options Group NewLine True Inline Inherit (LPConstant 4) True False) input
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

case11b :: Assertion
case11b = expected
    @=? testStep (step 80 $ Options Group WithModuleName True Inline Inherit (LPConstant 4) True False) input
  where
    expected = unlines
        [ "module Herp where"
        , ""
        , "import           Control.Monad"
        , "import           Data.List      as List (concat, foldl, foldr, head, init, last,"
        , "                     length, map, null, reverse, tail, (++))"
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
case12 :: Assertion
case12 = expected
    @=? testStep (step 80 $ Options Group NewLine True Inline Inherit (LPConstant 2) True False) input'
  where
    input' = unlines
        [ "import Data.List (map)"
        ]

    expected = unlines
        [ "import Data.List"
        , "  (map)"
        ]

--------------------------------------------------------------------------------
case12b :: Assertion
case12b = expected
    @=? testStep (step 80 $ Options Group WithModuleName True Inline Inherit (LPConstant 2) True False) input'
  where
    input' = unlines
        [ "import Data.List (map)"
        ]

    expected = input'

--------------------------------------------------------------------------------
case13 :: Assertion
case13 = expected
    @=? testStep (step 80 $ Options None WithAlias True InlineWithBreak Inherit (LPConstant 4) True False) input'
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
case13b :: Assertion
case13b = expected
    @=? testStep (step 80 $ Options None WithModuleName True InlineWithBreak Inherit (LPConstant 4) True False) input'
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
      (step 80 $ Options None WithAlias True InlineWithBreak Inherit (LPConstant 10) True False) expected
  where
    expected = unlines
        [ "import qualified Data.List as List (concat, map, null, reverse, tail, (++))"
        ]

--------------------------------------------------------------------------------
case15 :: Assertion
case15 = expected
    @=? testStep (step 80 $ Options None AfterAlias True Multiline Inherit (LPConstant 4) True False) input'
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
    @=? testStep (step 80 $ Options None AfterAlias True Multiline Inherit (LPConstant 4) False False) input'
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
    @=? testStep (step 80 $ Options None AfterAlias True Multiline Inherit (LPConstant 4) True False) input'
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
    (step 40 $ Options None AfterAlias True InlineToMultiline Inherit (LPConstant 4) True False) input'
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
    (step 40 $ Options Global NewLine True InlineWithBreak RightAfter (LPConstant 17) True False) case19input
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

case19b :: Assertion
case19b = expected @=? testStep
    (step 40 $ Options File NewLine True InlineWithBreak RightAfter (LPConstant 17) True False) case19input
  where
    expected = unlines
           ----------------------------------------
        [ "import Prelude ()"
        , "import Prelude.Compat hiding"
        , "                 (foldMap)"
        , ""
        , "import Data.List"
        , "                 (foldl', intercalate,"
        , "                 intersperse)"
        ]

case19c :: Assertion
case19c = expected @=? testStep
    (step 40 $ Options File NewLine True InlineWithBreak RightAfter LPModuleName True False) case19input
  where
    expected = unlines
           ----------------------------------------
        [ "import Prelude ()"
        , "import Prelude.Compat hiding"
        , "       (foldMap)"
        , ""
        , "import Data.List"
        , "       (foldl', intercalate,"
        , "       intersperse)"
        ]

case19d :: Assertion
case19d = expected @=? testStep
    (step 40 $ Options Global NewLine True InlineWithBreak RightAfter LPModuleName True False) case19input
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

case19input :: String
case19input = unlines
        [ "import Prelude.Compat hiding (foldMap)"
        , "import Prelude ()"
        , ""
        , "import Data.List (foldl', intercalate, intersperse)"
        ]

--------------------------------------------------------------------------------
case20 :: Assertion
case20 = expected
    @=? testStep (step 80 defaultOptions) input'
  where
    expected = unlines
        [ "import {-# SOURCE #-} Data.ByteString as BS"
        , "import qualified Data.Map        as Map"
        , "import           Data.Set        (empty)"
        , "import {-# SOURCE #-} qualified Data.Text       as T"
        ]
    input' = unlines
        [ "import {-# SOURCE #-}    Data.ByteString as BS"
        , "import {-# SOURCE #-} qualified Data.Text as T"
        , "import qualified   Data.Map as Map"
        , "import Data.Set (empty)"
        ]

--------------------------------------------------------------------------------
case21 :: Assertion
case21 = expected
    @=? testStep (step 80 defaultOptions) input'
  where
    expected = unlines
        [ "{-# LANGUAGE ExplicitNamespaces #-}"
        , "import           X1 (A, B, C)"
        , "import           X2 (A, B, C)"
        , "import           X3 (A (..))"
        , "import           X4 (A (..))"
        , "import           X5 (A (..))"
        , "import           X6 (A (a, b, c), B (m, n, o))"
        , "import           X7 (a, b, c)"
        , "import           X8 (type (+), (+))"
        , "import           X9 hiding (x, y, z)"
        ]
    input' = unlines
        [ "{-# LANGUAGE ExplicitNamespaces #-}"
        , "import X1 (A, B, A, C, A, B, A)"
        , "import X2 (C(), B(), A())"
        , "import X3 (A(..))"
        , "import X4 (A, A(..))"
        , "import X5 (A(..), A(x))"
        , "import X6 (A(a,b), B(m,n), A(c), B(o))"
        , "import X7 (a, b, a, c)"
        , "import X8 (type (+), (+))"
        , "import X9 hiding (x, y, z, x)"
        ]

--------------------------------------------------------------------------------
case22 :: Assertion
case22 = expected
    @=? testStep (step 80 defaultOptions) input'
  where
    expected = unlines
        [ "{-# LANGUAGE PackageImports #-}"
        , "import           A"
        , "import           \"blah\" A"
        , "import           \"foo\" A"
        , "import qualified \"foo\" A  as X"
        , "import           \"foo\" B  (shortName, someLongName, someLongerName,"
        , "                           theLongestNameYet)"
        ]
    input' = unlines
        [ "{-# LANGUAGE PackageImports #-}"
        , "import A"
        , "import \"foo\" A"
        , "import \"blah\" A"
        , "import qualified \"foo\" A as X"
        -- this import fits into 80 chats without "foo",
        -- but doesn't fit when "foo" is included into the calculation
        , "import \"foo\" B (someLongName, someLongerName, " ++
          "theLongestNameYet, shortName)"
        ]


--------------------------------------------------------------------------------
case23 :: Assertion
case23 = expected
    @=? testStep (step 40 $ Options None AfterAlias False Inline Inherit (LPConstant 4) True True) input'
  where
    expected = unlines
        [ "import Data.Acid ( AcidState )"
        , "import Data.Default.Class ( Default (def) )"
        , ""
        , "import Data.Monoid ( (<>) )"
        , ""
        , "import Data.ALongName.Foo ( Boo, Foo,"
        , "                            Goo )"
        ]

    input' = unlines
        [ "import Data.Acid (AcidState)"
        , "import Data.Default.Class (Default(def))"
        , ""
        , "import Data.Monoid ((<>) )"
        , ""
        , "import Data.ALongName.Foo (Foo, Goo, Boo)"
        ]

--------------------------------------------------------------------------------
case23b :: Assertion
case23b = expected
    @=? testStep (step 40 $ Options None WithModuleName False Inline Inherit (LPConstant 4) True True) input'
  where
    expected = unlines
        [ "import Data.Acid ( AcidState )"
        , "import Data.Default.Class"
        , "           ( Default (def) )"
        , ""
        , "import Data.Monoid ( (<>) )"
        , ""
        , "import Data.ALongName.Foo ( Boo, Foo,"
        , "           Goo )"
        ]

    input' = unlines
        [ "import Data.Acid (AcidState)"
        , "import Data.Default.Class (Default(def))"
        , ""
        , "import Data.Monoid ((<>) )"
        , ""
        , "import Data.ALongName.Foo (Foo, Goo, Boo)"
        ]

--------------------------------------------------------------------------------
case24 :: Assertion
case24 = expected
    @=? testStep (step 40 $ Options None AfterAlias False InlineWithBreak Inherit (LPConstant 4) True True) input'
  where
    expected = unlines
        [ "import Data.Acid ( AcidState )"
        , "import Data.Default.Class"
        , "    ( Default (def) )"
        , ""
        , "import Data.ALongName.Foo"
        , "    ( BooReallyLong, FooReallyLong,"
        , "    GooReallyLong )"
        ]

    input' = unlines
        [ "import Data.Acid (AcidState)"
        , "import Data.Default.Class (Default(def))"
        , ""
        , "import Data.ALongName.Foo (FooReallyLong, " ++
          "GooReallyLong, BooReallyLong)"
        ]

--------------------------------------------------------------------------------
case25 :: Assertion
case25 = expected
    @=? testStep (step 80 $ Options Group AfterAlias False Multiline Inherit (LPConstant 4) False False) input'
  where
    expected = unlines
        [ "import Data.Acid (AcidState)"
        , "import Data.Default.Class (Default(def))"
        , ""
        , "import           Data.Maybe (Maybe(Just, Nothing))"
        , "import qualified Data.Maybe.Extra (Maybe(Just, Nothing))"
        , ""
        , "import Data.Foo (Foo(Bar, Foo), Goo(Goo))"
        ]
    input' = unlines
        [ "import Data.Acid (AcidState)"
        , "import Data.Default.Class (Default(def))"
        , ""
        , "import Data.Maybe (Maybe   (Just, Nothing))"
        , "import qualified Data.Maybe.Extra (Maybe(Just, Nothing))"
        , ""
        , "import Data.Foo (Foo (Foo,Bar), Goo(Goo))"
        ]

--------------------------------------------------------------------------------
case26 :: Assertion
case26 = expected
    @=? testStep (step 80 options ) input'
  where
    options = defaultOptions { listAlign = NewLine, longListAlign = Multiline }
    input' = unlines
        [ "import Data.List"
        ]
    expected = unlines
        [ "import           Data.List"
        ]
