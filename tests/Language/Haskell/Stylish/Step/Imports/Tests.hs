--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists #-}
module Language.Haskell.Stylish.Step.Imports.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                        (Test, testGroup)
import           Test.Framework.Providers.HUnit        (testCase)
import           Test.HUnit                            (Assertion, (@=?))


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
    , testCase "case 27" case27
    , testCase "case 28" case28
    , testCase "case 29" case29
    , testCase "case 30" case30
    ]


--------------------------------------------------------------------------------
input :: Snippet
input =
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
case01 = assertSnippet (step (Just 80) $ fromImportAlign Global) input
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
case02 = assertSnippet (step (Just 80) $ fromImportAlign Group) input
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
case03 = assertSnippet (step (Just 80) $ fromImportAlign None) input
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
case04 = assertSnippet (step (Just 80) $ fromImportAlign Global)
    [ "import Data.Aeson.Types (object, typeMismatch, FromJSON(..)," ++
        "ToJSON(..), Value(..), parseEither, (.!=), (.:), (.:?), (.=))"
    ]
    [ "import           Data.Aeson.Types (FromJSON (..), ToJSON (..), Value (..),"
    , "                                   object, parseEither, typeMismatch, (.!=),"
    , "                                   (.:), (.:?), (.=))"
    ]


--------------------------------------------------------------------------------
case05 :: Assertion
case05 = assertSnippet (step (Just 80) $ fromImportAlign Group) input' input'
  where
    -- Putting this on a different line shouldn't really help.
    input' = ["import Distribution.PackageDescription.Configuration " ++
        "(finalizePackageDescription)"]


--------------------------------------------------------------------------------
case06 :: Assertion
case06 = assertSnippet (step (Just 80) $ fromImportAlign File) input' input'
  where
    input' =
        [ "import Bar.Qux"
        , "import Foo.Bar"
        ]


--------------------------------------------------------------------------------
case07 :: Assertion
case07 = assertSnippet (step (Just 80) $ fromImportAlign File)
    [ "import Bar.Qux"
    , ""
    , "import qualified Foo.Bar"
    ]
    [ "import           Bar.Qux"
    , ""
    , "import qualified Foo.Bar"
    ]


--------------------------------------------------------------------------------
case08 :: Assertion
case08 =
  let
    options = Options Global WithAlias True Inline Inherit (LPConstant 4) True False
  in
    assertSnippet (step (Just 80) options) input
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
case08b =
  let
    options = Options Global WithModuleName True Inline Inherit (LPConstant 4) True False
  in
    assertSnippet (step (Just 80) options) input
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
case09 =
  let
    options = Options Global WithAlias True Multiline Inherit (LPConstant 4) True False
  in
    assertSnippet (step (Just 80) options) input
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
case10 =
  let
    options = Options Group WithAlias True Multiline Inherit (LPConstant 4) True False
  in
    assertSnippet (step (Just 40) options) input
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
case11 =
  let
    options = Options Group NewLine True Inline Inherit (LPConstant 4) True False
  in
    assertSnippet (step (Just 80) options) input
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
case11b =
  let
    options = Options Group WithModuleName True Inline Inherit (LPConstant 4) True False
  in
    assertSnippet (step (Just 80) options) input
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
case12 =
  let
    options = Options Group NewLine True Inline Inherit (LPConstant 2) True False
  in
    assertSnippet (step (Just 80) options)
    [ "import Data.List (map)"
    ]
    [ "import Data.List"
    , "  (map)"
    ]


--------------------------------------------------------------------------------
case12b :: Assertion
case12b =
  let
    options = Options Group WithModuleName True Inline Inherit (LPConstant 2) True False
  in
    assertSnippet (step (Just 80) options)
    ["import Data.List (map)"]
    ["import Data.List (map)"]


--------------------------------------------------------------------------------
case13 :: Assertion
case13 =
  let
    options = Options None WithAlias True InlineWithBreak Inherit (LPConstant 4) True False
  in
    assertSnippet (step (Just 80) options)
    [ "import qualified Data.List as List (concat, foldl, foldr, head, init,"
    , "    last, length, map, null, reverse, tail, (++))"
    ]
    [ "import qualified Data.List as List"
    , "    (concat, foldl, foldr, head, init, last, length, map, null, reverse, tail,"
    , "    (++))"
    ]

case13b :: Assertion
case13b =
  let
    options = Options None WithModuleName True InlineWithBreak Inherit (LPConstant 4) True False
  in
    assertSnippet (step (Just 80) options)
    [ "import qualified Data.List as List (concat, foldl, foldr, head, init,"
    , "    last, length, map, null, reverse, tail, (++))"
    ]
    [ "import qualified Data.List as List"
    , "    (concat, foldl, foldr, head, init, last, length, map, null, reverse, tail,"
    , "    (++))"
    ]


--------------------------------------------------------------------------------
case14 :: Assertion
case14 =
  let
    options = Options None WithAlias True InlineWithBreak Inherit (LPConstant 10) True False
  in
    assertSnippet (step (Just 80) options)
    [ "import qualified Data.List as List (concat, map, null, reverse, tail, (++))"
    ]
    [ "import qualified Data.List as List (concat, map, null, reverse, tail, (++))"
    ]


--------------------------------------------------------------------------------
case15 :: Assertion
case15 =
  let
    options = Options None AfterAlias True Multiline Inherit (LPConstant 4) True False
  in
    assertSnippet (step (Just 80) options)
    [ "import Data.Acid (AcidState)"
    , "import qualified Data.Acid as Acid (closeAcidState, createCheckpoint, openLocalStateFrom)"
    , "import Data.Default.Class (Default (def))"
    , ""
    , "import qualified Herp.Derp.Internal.Types.Foobar as Internal (foo, bar)"
    ]
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


--------------------------------------------------------------------------------
case16 :: Assertion
case16 =
  let
    options = Options None AfterAlias True Multiline Inherit (LPConstant 4) False False
  in
    assertSnippet (step (Just 80) options)
    [ "import Data.Acid (AcidState)"
    , "import Data.Default.Class (Default(def))"
    , ""
    , "import Data.Maybe (Maybe   (Just, Nothing))"
    , ""
    , "import Data.Foo (Foo (Foo,Bar), Goo(Goo))"
    ]
    [ "import Data.Acid (AcidState)"
    , "import Data.Default.Class (Default(def))"
    , ""
    , "import Data.Maybe (Maybe(Just, Nothing))"
    , ""
    , "import Data.Foo (Foo(Bar, Foo), Goo(Goo))"
    ]


--------------------------------------------------------------------------------
case17 :: Assertion
case17 =
  let
    options = Options None AfterAlias True Multiline Inherit (LPConstant 4) True False
  in
    assertSnippet (step (Just 80) options)
    [ "import Control.Applicative (Applicative ((<*>),pure))"
    , ""
    , "import Data.Identity (Identity (runIdentity,Identity))"
    ]
    [ "import Control.Applicative (Applicative (pure, (<*>)))"
    , ""
    , "import Data.Identity (Identity (Identity, runIdentity))"
    ]


--------------------------------------------------------------------------------
case18 :: Assertion
case18 =
  let
    options = Options None AfterAlias True InlineToMultiline Inherit (LPConstant 4) True False
  in
    assertSnippet (step (Just 40) options)
    [ "import Data.Foo as Foo (Bar, Baz, Foo)"
    , ""
    , "import Data.Identity (Identity (Identity, runIdentity))"
    , ""
    , "import Data.Acid as Acid (closeAcidState, createCheckpoint, openLocalStateFrom)"
    ]
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


--------------------------------------------------------------------------------
case19 :: Assertion
case19 =
  let
    options = Options Global NewLine True InlineWithBreak RightAfter (LPConstant 17) True False
  in
    assertSnippet (step (Just 40) options) case19input
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
case19b =
  let
    options = Options File NewLine True InlineWithBreak RightAfter (LPConstant 17) True False
  in
    assertSnippet (step (Just 40) options) case19input
       ----------------------------------------
    [ "import Prelude ()"
    , "import Prelude.Compat hiding (foldMap)"
    , ""
    , "import Data.List"
    , "                 (foldl', intercalate,"
    , "                 intersperse)"
    ]

case19c :: Assertion
case19c =
  let
    options = Options File NewLine True InlineWithBreak RightAfter LPModuleName True False
  in
    assertSnippet (step (Just 40) options) case19input
       ----------------------------------------
    [ "import Prelude ()"
    , "import Prelude.Compat hiding (foldMap)"
    , ""
    , "import Data.List"
    , "       (foldl', intercalate,"
    , "       intersperse)"
    ]

case19d :: Assertion
case19d =
  let
    options = Options Global NewLine True InlineWithBreak RightAfter LPModuleName True False
  in
    assertSnippet (step (Just 40) options) case19input
       ----------------------------------------
    [ "import           Prelude ()"
    , "import           Prelude.Compat hiding"
    , "                 (foldMap)"
    , ""
    , "import           Data.List"
    , "                 (foldl', intercalate,"
    , "                 intersperse)"
    ]

case19input :: Snippet
case19input = Snippet
    [ "import Prelude.Compat hiding (foldMap)"
    , "import Prelude ()"
    , ""
    , "import Data.List (foldl', intercalate, intersperse)"
    ]


--------------------------------------------------------------------------------
case20 :: Assertion
case20 = expected
    @=? testSnippet (step (Just 80) defaultOptions) input'
  where
    expected = Snippet
        [ "import {-# SOURCE #-}           Data.ByteString as BS"
        , "import                qualified Data.Map        as Map"
        , "import                          Data.Set        (empty)"
        , "import {-# SOURCE #-} qualified Data.Text       as T"
        ]
    input' = Snippet
        [ "import {-# SOURCE #-}    Data.ByteString as BS"
        , "import {-# SOURCE #-} qualified Data.Text as T"
        , "import qualified   Data.Map as Map"
        , "import Data.Set (empty)"
        ]


--------------------------------------------------------------------------------
case21 :: Assertion
case21 =
    assertSnippet (step (Just 80) defaultOptions)
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


--------------------------------------------------------------------------------
case22 :: Assertion
case22 = assertSnippet (step (Just 80) defaultOptions)
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
    [ "{-# LANGUAGE PackageImports #-}"
    , "import           A"
    , "import           \"blah\" A"
    , "import           \"foo\" A"
    , "import qualified \"foo\" A  as X"
    , "import           \"foo\" B  (shortName, someLongName, someLongerName,"
    , "                           theLongestNameYet)"
    ]


--------------------------------------------------------------------------------
case23 :: Assertion
case23 =
  let
    options = Options None AfterAlias False Inline Inherit (LPConstant 4) True True
  in
    assertSnippet (step (Just 40) options)
    [ "import Data.Acid (AcidState)"
    , "import Data.Default.Class (Default(def))"
    , ""
    , "import Data.Monoid ((<>) )"
    , ""
    , "import Data.ALongName.Foo (Foo, Goo, Boo)"
    ]
       ----------------------------------------
    [ "import Data.Acid ( AcidState )"
    , "import Data.Default.Class ( Default (def) )"
    , ""
    , "import Data.Monoid ( (<>) )"
    , ""
    , "import Data.ALongName.Foo ( Boo, Foo,"
    , "                            Goo )"
    ]


--------------------------------------------------------------------------------
case23b :: Assertion
case23b =
  let
    options = Options None WithModuleName False Inline Inherit (LPConstant 4) True True
  in
    assertSnippet (step (Just 40) options)
    [ "import Data.Acid (AcidState)"
    , "import Data.Default.Class (Default(def))"
    , ""
    , "import Data.Monoid ((<>) )"
    , ""
    , "import Data.ALongName.Foo (Foo, Goo, Boo)"
    ]
       ----------------------------------------
    [ "import Data.Acid ( AcidState )"
    , "import Data.Default.Class"
    , "           ( Default (def) )"
    , ""
    , "import Data.Monoid ( (<>) )"
    , ""
    , "import Data.ALongName.Foo ( Boo, Foo,"
    , "           Goo )"
    ]


--------------------------------------------------------------------------------
case24 :: Assertion
case24 =
  let
    options = Options None AfterAlias False InlineWithBreak Inherit (LPConstant 4) True True
  in
    assertSnippet (step (Just 40) options)
    [ "import Data.Acid (AcidState)"
    , "import Data.Default.Class (Default(def))"
    , ""
    , "import Data.ALongName.Foo (FooReallyLong, " ++
      "GooReallyLong, BooReallyLong)"
    ]
       ----------------------------------------
    [ "import Data.Acid ( AcidState )"
    , "import Data.Default.Class"
    , "    ( Default (def) )"
    , ""
    , "import Data.ALongName.Foo"
    , "    ( BooReallyLong, FooReallyLong,"
    , "    GooReallyLong )"
    ]


--------------------------------------------------------------------------------
case25 :: Assertion
case25 =
  let
    options = Options Group AfterAlias False Multiline Inherit (LPConstant 4) False False
  in
    assertSnippet (step (Just 80) options)
    [ "import Data.Acid (AcidState)"
    , "import Data.Default.Class (Default(def))"
    , ""
    , "import Data.Maybe (Maybe   (Just, Nothing))"
    , "import qualified Data.Maybe.Extra (Maybe(Just, Nothing))"
    , ""
    , "import Data.Foo (Foo (Foo,Bar), Goo(Goo))"
    ]
    [ "import Data.Acid (AcidState)"
    , "import Data.Default.Class (Default(def))"
    , ""
    , "import           Data.Maybe (Maybe(Just, Nothing))"
    , "import qualified Data.Maybe.Extra (Maybe(Just, Nothing))"
    , ""
    , "import Data.Foo (Foo(Bar, Foo), Goo(Goo))"
    ]


--------------------------------------------------------------------------------
case26 :: Assertion
case26 =
    assertSnippet (step (Just 80) options)
    ["import Data.List"]
    ["import           Data.List"]
  where
    options = defaultOptions { listAlign = NewLine, longListAlign = Multiline }


--------------------------------------------------------------------------------
case27 :: Assertion
case27 = assertSnippet (step Nothing $ fromImportAlign Global) input
    [ "module Herp where"
    , ""
    , "import           Control.Monad"
    , "import           Data.List           as List (concat, foldl, foldr, head, init, last, length, map, null, reverse, tail, (++))"
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
case28 :: Assertion
case28 = assertSnippet (step (Just 80) $ fromImportAlign Global)
    [ "import Data.Default.Class (Default(def))"
    , "import qualified Data.Aeson as JSON"
    , "import qualified Data.Aeson as JSON"
    , "import Control.Monad"
    , "import Control.Monad"
    , ""
    , "import Data.Maybe (Maybe   (Just, Nothing))"
    , "import qualified Data.Maybe.Extra (Maybe(Just, Nothing))"
    , ""
    , "import Data.Foo (Foo (Foo,Bar), Goo(Goo))"
    , "import Data.Foo (Foo (Foo,Bar))"
    , "import Data.Set (empty, intersect)"
    , "import Data.Set (empty, nub)"
    ]
    [ "import           Control.Monad"
    ,  "import qualified Data.Aeson         as JSON"
    , "import           Data.Default.Class (Default (def))"
    , ""
    , "import           Data.Maybe         (Maybe (Just, Nothing))"
    , "import qualified Data.Maybe.Extra   (Maybe (Just, Nothing))"
    , ""
    , "import           Data.Foo           (Foo (Bar, Foo), Goo (Goo))"
    , "import           Data.Set           (empty, intersect, nub)"
    ]


--------------------------------------------------------------------------------
case29 :: Assertion
case29 = assertSnippet (step Nothing $ fromImportAlign Group)
    -- Check that "Group" mode recognizes groups with multi-line imports
    [ "import Foo (foo)"
    , "import BarBar ( bar"
    , "              , kek)"
    , "import Abcd ()"
    , ""
    , "import A (A)"
    ]
    [ "import Abcd   ()"
    , "import BarBar (bar, kek)"
    , "import Foo    (foo)"
    , ""
    , "import A (A)"
    ]


--------------------------------------------------------------------------------
case30 :: Assertion
case30 = assertSnippet (step Nothing defaultOptions {separateLists = False})
    ["import           Data.Monoid (Monoid (..))"]
    ["import           Data.Monoid (Monoid(..))"]
