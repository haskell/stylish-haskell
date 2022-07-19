{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists   #-}
module Language.Haskell.Stylish.Step.Imports.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                        (Test, testGroup)
import           Test.Framework.Providers.HUnit        (testCase)
import           Test.HUnit                            (Assertion)


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
    , testCase "case 26 (issue #185)" case26
    , testCase "case 27" case27
    , testCase "case 28" case28
    , testCase "case 29" case29
    , testCase "case 30" case30
    , testCase "case 31" case31
    , testCase "case 32" case32
    , testCase "case 33" case33
    , testCase "case 34" case34
    , testCase "case 35" case35
    , testCase "case 36" case36
    , testCase "case 37" case37
    , testCase "case 38" case38
    , testCase "case 39" case39
    , testCase "case 40" case40
    , testCase "case 41" case41
    , testCase "case 42" case42
    , testCase "case 43" case43
    , testCase "case 44a" case44a
    , testCase "case 44b" case44b
    , testCase "case 44c" case44c
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
    options = defaultOptions { listAlign = WithAlias }
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
    options = defaultOptions { listAlign = WithModuleName }
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
    options = defaultOptions { listAlign = WithAlias, longListAlign = Multiline }
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
    options = defaultOptions
      { importAlign   = Group
      , listAlign     = WithAlias
      , longListAlign = Multiline
      }
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
    options = defaultOptions { importAlign = Group, listAlign = NewLine }
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
    options = defaultOptions { importAlign = Group, listAlign = WithModuleName }
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
    options = defaultOptions
      { importAlign = Group
      , listAlign   = NewLine
      , listPadding = LPConstant 2
      }
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
    options = defaultOptions
      { importAlign = Group
      , listAlign   = WithModuleName
      , listPadding = LPConstant 2
      }
  in
    assertSnippet (step (Just 80) options)
    ["import Data.List (map)"]
    ["import Data.List (map)"]


--------------------------------------------------------------------------------
case13 :: Assertion
case13 =
  let
    options = defaultOptions
      { importAlign   = None
      , listAlign     = WithAlias
      , longListAlign = InlineWithBreak
      }
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
    options = defaultOptions
      { importAlign   = None
      , listAlign     = WithModuleName
      , longListAlign = InlineWithBreak
      }
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
    options = defaultOptions
      { importAlign   = None
      , listAlign     = WithAlias
      , longListAlign = InlineWithBreak
      , listPadding   = LPConstant 10
      }
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
    options = defaultOptions { importAlign = None, longListAlign = Multiline }
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
    options = defaultOptions
      { importAlign   = None
      , longListAlign = Multiline
      , separateLists = False
      }
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
    options = defaultOptions { importAlign = None, longListAlign = Multiline }
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
    options = defaultOptions { importAlign = None, longListAlign = InlineToMultiline }
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
    options = defaultOptions
      { listAlign      = NewLine
      , longListAlign  = InlineWithBreak
      , emptyListAlign = RightAfter
      , listPadding    = LPConstant 17
      }
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
    options = defaultOptions
      { importAlign    = File
      , listAlign      = NewLine
      , longListAlign  = InlineWithBreak
      , emptyListAlign = RightAfter
      , listPadding    = LPConstant 17
      }
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
    options = defaultOptions
      { importAlign    = File
      , listAlign      = NewLine
      , longListAlign  = InlineWithBreak
      , emptyListAlign = RightAfter
      , listPadding    = LPModuleName
      }
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
    options = defaultOptions
      { listAlign      = NewLine
      , longListAlign  = InlineWithBreak
      , emptyListAlign = RightAfter
      , listPadding    = LPModuleName
      }
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
case20 = assertSnippet (step (Just 80) defaultOptions)
    [ "import {-# SOURCE #-}    Data.ByteString as BS"
    , "import {-# SOURCE #-} qualified Data.Text as T"
    , "import qualified   Data.Map as Map"
    , "import Data.Set (empty)"
    ]
    [ "import {-# SOURCE #-}           Data.ByteString as BS"
    , "import                qualified Data.Map        as Map"
    , "import                          Data.Set        (empty)"
    , "import {-# SOURCE #-} qualified Data.Text       as T"
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
    options = defaultOptions
      { importAlign    = None
      , padModuleNames = False
      , spaceSurround  = True
      }
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
    options = defaultOptions
      { importAlign    = None
      , listAlign      = WithModuleName
      , padModuleNames = False
      , spaceSurround  = True
      }
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
    options = defaultOptions
      { importAlign    = None
      , padModuleNames = False
      , longListAlign  = InlineWithBreak
      , spaceSurround  = True
      }
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
    options = defaultOptions
      { importAlign    = Group
      , padModuleNames = False
      , longListAlign  = Multiline
      , separateLists  = False
      }
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
    , "import qualified Data.Aeson         as JSON"
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

--------------------------------------------------------------------------------
case31 :: Assertion
case31 = assertSnippet (step Nothing defaultOptions {postQualified = True})
    ["import           Data.Monoid (Monoid (..))"]
    ["import Data.Monoid (Monoid (..))"]

--------------------------------------------------------------------------------
case32 :: Assertion
case32 = assertSnippet (step Nothing defaultOptions {postQualified = True})
    ["import qualified Data.Monoid as M"]
    ["import Data.Monoid qualified as M"]

--------------------------------------------------------------------------------
case33 :: Assertion
case33 = assertSnippet (step Nothing defaultOptions {postQualified = True})
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
    [ "import Control.Monad"
    , "import Data.Aeson         qualified as JSON"
    , "import Data.Default.Class (Default (def))"
    , ""
    , "import Data.Maybe         (Maybe (Just, Nothing))"
    , "import Data.Maybe.Extra   qualified (Maybe (Just, Nothing))"
    , ""
    , "import Data.Foo           (Foo (Bar, Foo), Goo (Goo))"
    , "import Data.Set           (empty, intersect, nub)"
    ]

--------------------------------------------------------------------------------
case34 :: Assertion
case34 = assertSnippet (step Nothing defaultOptions {postQualified = True})
    [ "import qualified Data.Aeson as JSON (Value)"
    ]
    [ "import Data.Aeson qualified as JSON (Value)"
    ]

--------------------------------------------------------------------------------
case35 :: Assertion
case35 = assertSnippet (step Nothing defaultOptions {postQualified = True})
    [ "import Data.Aeson qualified as JSON (Value)"
    ]
    [ "import Data.Aeson qualified as JSON (Value)"
    ]

--------------------------------------------------------------------------------
case36 :: Assertion
case36 = assertSnippet (step Nothing defaultOptions {postQualified = True})
    [ "import qualified Data.Aeson as JSON (Value)"
    , "import qualified Data.Aeson as JSON (encode, decode)"
    ]
    [ "import Data.Aeson qualified as JSON (Value, decode, encode)"
    ]

--------------------------------------------------------------------------------
case37 :: Assertion
case37 = assertSnippet (step Nothing defaultOptions {postQualified = True})
    [ "import Data.Aeson qualified as JSON (Value)"
    , "import Data.Aeson qualified as JSON (encode, decode)"
    ]
    [ "import Data.Aeson qualified as JSON (Value, decode, encode)"
    ]

--------------------------------------------------------------------------------
case38 :: Assertion
case38 = assertSnippet (step (Just 80) $ fromImportAlign File)
    [ "import HSP"
    , "import Happstack.Server"
    ]
    [ "import Happstack.Server"
    , "import HSP"
    ]

--------------------------------------------------------------------------------
case39 :: Assertion
case39 = assertSnippet (step Nothing options)
  [ "import Something.A"
  , "import SomethingElse.A"
  , "import SomeThing.B"
  , "import SomeThingelse.B"
  ]
  [ "import           SomeThing.B"
  , ""
  , "import           SomeThingelse.B"
  , ""
  , "import           Something.A"
  , ""
  , "import           SomethingElse.A"
  ]
  where options = defaultOptions { groupImports = True }

--------------------------------------------------------------------------------
case40 :: Assertion
case40 = assertSnippet (step Nothing options)
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
    [ "import Control.Monad"
    , ""
    , "import Data.Aeson         qualified as JSON"
    , "import Data.Default.Class (Default (def))"
    , "import Data.Foo           (Foo (Bar, Foo), Goo (Goo))"
    , "import Data.Maybe         (Maybe (Just, Nothing))"
    , "import Data.Maybe.Extra   qualified (Maybe (Just, Nothing))"
    , "import Data.Set           (empty, intersect, nub)"
    ]
  where options = defaultOptions { groupImports = True, postQualified = True }

--------------------------------------------------------------------------------
case41 :: Assertion
case41 = assertSnippet (step Nothing options)
    [ "import Data.Default.Class (Default(def))"
    , "import qualified Data.Aeson as JSON"
    , "import Control.Monad"
    , "import Control.Monad"
    , "import qualified Foo.Bar.Baz"
    , ""
    , "import Data.Set (empty, intersect)"
    , "import Data.Maybe (Maybe   (Just, Nothing))"
    , "import qualified Data.Maybe.Extra (Maybe(Just, Nothing))"
    , ""
    , "import qualified Data.Aeson as JSON"
    , ""
    , "import Data.Foo (Foo (Foo,Bar), Goo(Goo))"
    , "import Data.Foo (Foo (Foo,Bar))"
    , "import Data.Set (empty, nub)"
    , "import Foo.Bar.Baz (Foo)"
    ]
    [ "import Control.Monad"
    , ""
    , "import qualified Data.Aeson         as JSON"
    , "import           Data.Default.Class (Default (def))"
    , "import           Data.Foo           (Foo (Bar, Foo), Goo (Goo))"
    , "import           Data.Maybe         (Maybe (Just, Nothing))"
    , "import qualified Data.Maybe.Extra   (Maybe (Just, Nothing))"
    , "import           Data.Set           (empty, intersect, nub)"
    , ""
    , "import           Foo.Bar.Baz (Foo)"
    , "import qualified Foo.Bar.Baz"
    ]
  where options = defaultOptions { groupImports = True, importAlign = Group }

--------------------------------------------------------------------------------
case42 :: Assertion
case42 =
    assertSnippet (step (Just 80) options)
    [ "import Data.Acid (AcidState)"
    , "import Data.Default.Class (Default (def))"
    , "import Control.Monad"
    , ""
    , "import qualified Data.Acid as Acid (closeAcidState, createCheckpoint, openLocalStateFrom)"
    , ""
    , "import qualified Herp.Derp.Internal.Types.Foobar as Internal (foo, bar)"
    ]
    [ "import Control.Monad"
    , ""
    , "import Data.Acid (AcidState)"
    , "import qualified Data.Acid as Acid"
    , "    ( closeAcidState"
    , "    , createCheckpoint"
    , "    , openLocalStateFrom"
    , "    )"
    , "import Data.Default.Class (Default (def))"
    , ""
    , "import qualified Herp.Derp.Internal.Types.Foobar as Internal (bar, foo)"
    ]
  where options = defaultOptions
          { groupImports  = True
          , importAlign   = None
          , longListAlign = Multiline
          }

--------------------------------------------------------------------------------
case43 :: Assertion
case43 =
    assertSnippet (step (Just 80) options)
    [ "import Project.Internal.Blah"
    , "import Project.Something"
    , "import Control.Monad"
    , ""
    , "import qualified Data.Acid as Acid (closeAcidState, createCheckpoint, openLocalStateFrom)"
    , ""
    , "import qualified Project.Internal.Blarg as Blarg"
    , "import Control.Applicative"
    , "import Data.Functor"
    , "import Data.Acid (AcidState)"
    , "import Project"
    , ""
    , "import Data.Map (Map)"
    , "import qualified Data.Map as Map"
    ]
    [ "import Project.Internal.Blah"
    , "import qualified Project.Internal.Blarg as Blarg"
    , ""
    , "import Project"
    , "import Project.Something"
    , ""
    , "import Control.Applicative"
    , "import Control.Monad"
    , ""
    , "import Data.Acid (AcidState)"
    , "import qualified Data.Acid as Acid"
    , "    ( closeAcidState"
    , "    , createCheckpoint"
    , "    , openLocalStateFrom"
    , "    )"
    , "import Data.Functor"
    , "import Data.Map (Map)"
    , "import qualified Data.Map as Map"
    ]
  where options = defaultOptions
          { groupImports  = True
          , groupRules    =
              [ GroupRule
                { match = unsafeParsePattern "Project\\.Internal"
                , subGroup = Nothing
                }
              , GroupRule
                { match = unsafeParsePattern "Project"
                , subGroup = Nothing
                }
              , GroupRule
                { match = unsafeParsePattern ".*"
                , subGroup = Just $ unsafeParsePattern "^[^.]+"
                }
              ]
          , importAlign   = None
          , longListAlign = Multiline
          }

--------------------------------------------------------------------------------
case44a :: Assertion
case44a =
    assertSnippet (step (Just 80) options)
    [ "import Project"
    , "import Control.Monad"
    , ""
    , "import qualified Data.Acid as Acid"
    , "import Project.Something"
    , "import Data.Default.Class (Default (def))"
    , ""
    , "import qualified Herp.Derp.Internal.Types.Foobar as Internal (foo, bar)"
    , "import ProJect.WrongCapitalization"
    ]
    [ "import Project"
    , "import Project.Something"
    , ""
    , "import Control.Monad"
    , "import qualified Data.Acid as Acid"
    , "import Data.Default.Class (Default (def))"
    , "import qualified Herp.Derp.Internal.Types.Foobar as Internal (bar, foo)"
    , "import ProJect.WrongCapitalization"
    ]
  where options = defaultOptions
          { groupImports = True
          , groupRules   = [ GroupRule
                             { match = unsafeParsePattern "Project"
                             , subGroup = Nothing
                             }
                           ]
          , importAlign  = None
          }

--------------------------------------------------------------------------------
case44b :: Assertion
case44b =
    assertSnippet (step (Just 80) options)
    [ "import Project"
    , "import Control.Monad"
    , ""
    , "import qualified Data.Acid as Acid"
    , "import Project.Something"
    , "import Data.Default.Class (Default (def))"
    , ""
    , "import qualified Herp.Derp.Internal.Types.Foobar as Internal (foo, bar)"
    , "import ProJect.WrongCapitalization"
    ]
    [ "import Project"
    , "import Project.Something"
    , ""
    , "import qualified Data.Acid as Acid"
    , ""
    , "import Data.Default.Class (Default (def))"
    , ""
    , "import qualified Herp.Derp.Internal.Types.Foobar as Internal (bar, foo)"
    , ""
    , "import Control.Monad"
    , ""
    , "import ProJect.WrongCapitalization"
    ]
  where options = defaultOptions
          { groupImports = True
          , groupRules   =
              [ GroupRule
                { match = unsafeParsePattern "Project"
                , subGroup = Nothing
                }
              , GroupRule
                { match    = unsafeParsePattern "[^.]+\\.[^.]+"
                , subGroup = Just $ unsafeParsePattern "\\.[^.]+"
                }
              ]
          , importAlign  = None
          }


--------------------------------------------------------------------------------
case44c :: Assertion
case44c =
    assertSnippet (step (Just 80) options)
    [ "import Project"
    , "import Control.Monad"
    , ""
    , "import qualified Data.Acid as Acid"
    , "import Project.Something"
    , "import Data.Default.Class (Default (def))"
    , ""
    , "import qualified Herp.Derp.Internal.Types.Foobar as Internal (foo, bar)"
    , "import ProJect.WrongCapitalization"
    ]
    [ "import Project"
    , "import Project.Something"
    , ""
    , "import Control.Monad"
    , "import qualified Data.Acid as Acid"
    , "import Data.Default.Class (Default (def))"
    , "import qualified Herp.Derp.Internal.Types.Foobar as Internal (bar, foo)"
    , "import ProJect.WrongCapitalization"
    ]
  where options = defaultOptions
          { groupImports = True
          , groupRules   =
              [ GroupRule
                { match = unsafeParsePattern "Project"
                , subGroup = Nothing
                }
              , GroupRule
                { match = unsafeParsePattern "[^.]+\\.[^.]+"
                , subGroup = Nothing
                }
              ]
          , importAlign  = None
          }
