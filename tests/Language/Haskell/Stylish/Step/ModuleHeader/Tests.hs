{-# LANGUAGE OverloadedLists #-}
module Language.Haskell.Stylish.Step.ModuleHeader.Tests
  ( tests
  ) where

--------------------------------------------------------------------------------
import           Prelude                                    hiding (lines)
import           Test.Framework                             (Test, testGroup)
import           Test.Framework.Providers.HUnit             (testCase)
import           Test.HUnit                                 (Assertion)

--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Step.ModuleHeader
import           Language.Haskell.Stylish.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Language.Haskell.Stylish.Printer.ModuleHeader"
    [ testCase "Does not indent absent export list" ex0
    , testCase "Empty exports list" ex1
    , testCase "Single exported variable" ex2
    , testCase "Multiple exported variables" ex3
    , testCase "Only reformats module header" ex4
    , testCase "Leaving pragmas in place" ex5
    , testCase "Leaving pragmas in place variant" ex6
    , testCase "Leaving comments in place" ex7
    , testCase "Exports all" ex8
    , testCase "Exports module" ex9
    , testCase "Exports symbol" ex10
    , testCase "Respects groups" ex11
    , testCase "'where' not repeated when not part of exports with always break_where" ex12
    , testCase "Indents absent export list with 2 spaces with always break_where" ex13
    , testCase "Indents with 2 spaces" ex14
    , testCase "Group doc with 2 spaces" ex15
    , testCase "Does not sort" ex16
    , testCase "Repects separate_lists" ex17
    , testCase "Indents absent export list with always break_where" ex18
    , testCase "Respects bundled patterns" ex19
    , testCase "Inline no export list" ex20
    , testCase "Inline empty export list" ex21
    , testCase "Inline single export" ex22
    , testCase "Inline single line sorts" ex23
    , testCase "Inline breaks when too long" ex24
    , testCase "Inline single line when no max cols" ex25
    , testCase "Inline breaks when comments present" ex26
    , testCase "Single no export list" ex27
    , testCase "Single empty export list" ex28
    , testCase "Single one export" ex29
    , testCase "Single two exports" ex30
    , testCase "Single one export with comment" ex31
    ]

--------------------------------------------------------------------------------
ex0 :: Assertion
ex0 = assertSnippet (step Nothing defaultConfig) input input
  where
    input =
      [ "module Foo where"
      ]

ex1 :: Assertion
ex1 = assertSnippet (step Nothing defaultConfig)
    [ "module Foo () where"
    ]
    [ "module Foo"
    , "    ("
    , "    ) where"
    ]

ex2 :: Assertion
ex2 = assertSnippet (step Nothing defaultConfig)
    [ "module Foo (tests) where"
    ]
    [ "module Foo"
    , "    ( tests"
    , "    ) where"
    ]

ex3 :: Assertion
ex3 = assertSnippet (step Nothing defaultConfig)
    [ "module Foo (t1, t2, t3) where"
    ]
    [ "module Foo"
    , "    ( t1"
    , "    , t2"
    , "    , t3"
    , "    ) where"
    ]

ex4 :: Assertion
ex4 = assertSnippet (step Nothing defaultConfig)
    [ "module Foo ("
    , "  t1,"
    , "  t3,"
    , "  t2"
    , ") where"
    , ""
    , ""
    , "-- | Docstring"
    , "foo :: Int"
    , "foo = 1"
    ]
    [ "module Foo"
    , "    ( t1"
    , "    , t2"
    , "    , t3"
    , "    ) where"
    , ""
    , ""
    , "-- | Docstring"
    , "foo :: Int"
    , "foo = 1"
    ]

ex5 :: Assertion
ex5 = assertSnippet (step Nothing defaultConfig)
    [ "{-# LANGUAGE DerivingVia #-}"
    , "-- | This module docs"
    , "module Foo ("
    , "  t1,"
    , "  t3,"
    , "  t2"
    , ") where"
    ]
    [ "{-# LANGUAGE DerivingVia #-}"
    , "-- | This module docs"
    , "module Foo"
    , "    ( t1"
    , "    , t2"
    , "    , t3"
    , "    ) where"
    ]

ex6 :: Assertion
ex6 = assertSnippet (step Nothing defaultConfig)
    [ "-- | This module docs"
    , "{-# LANGUAGE DerivingVia #-}"
    , "module Foo ("
    , "    t1,"
    , "    t3,"
    , "    t2"
    , ") where"
    ]
    [ "-- | This module docs"
    , "{-# LANGUAGE DerivingVia #-}"
    , "module Foo"
    , "    ( t1"
    , "    , t2"
    , "    , t3"
    , "    ) where"
    ]

ex7 :: Assertion
ex7 = assertSnippet (step Nothing defaultConfig)
    [ "module Foo -- Foo"
    , "("
    , " -- * t1 something"
    , "  t3,"
    , "  t1,"
    , " -- * t2 something"
    , "  t2"
    , ") where -- x"
    , "-- y"
    ]
    [ "module Foo -- Foo"
    , "    ( -- * t1 something"
    , "      t1"
    , "    , t3"
    , "      -- * t2 something"
    , "    , t2"
    , "    ) where -- x"
    , "-- y"
    ]


ex8 :: Assertion
ex8 = assertSnippet (step Nothing defaultConfig)
    [ "module Foo ("
    , " -- * t1 something"
    , "  t3,"
    , "  A(..),"
    , " -- * t2 something"
    , "  t2,"
    , "  t1"
    , ") where -- x"
    , "-- y"
    ]
    [ "module Foo"
    , "    ( -- * t1 something"
    , "      A (..)"
    , "    , t3"
    , "      -- * t2 something"
    , "    , t1"
    , "    , t2"
    , "    ) where -- x"
    , "-- y"
    ]

ex9 :: Assertion
ex9 = assertSnippet (step Nothing defaultConfig)
    [ "module Foo ("
    , " -- * t1 something"
    , "  module A,"
    , "  t3,"
    , " -- * t2 something"
    , "  t2"
    , ") where -- x"
    , "-- y"
    ]
    [ "module Foo"
    , "    ( -- * t1 something"
    , "      module A"
    , "    , t3"
    , "      -- * t2 something"
    , "    , t2"
    , "    ) where -- x"
    , "-- y"
    ]

ex10 :: Assertion
ex10 = assertSnippet (step Nothing defaultConfig)
    [ "module Foo ("
    , "  (<&>)"
    , ") where -- x"
    , "-- y"
    ]
    [ "module Foo"
    , "    ( (<&>)"
    , "    ) where -- x"
    , "-- y"
    ]

ex11 :: Assertion
ex11 = assertSnippet (step Nothing defaultConfig)
    [ "module Foo ("
    , "  -- group 1"
    , " g1_1,"
    , " g1_0,"
    , "  -- group 2"
    , " g0_1,"
    , " g0_0"
    , ") where"
    ]
    [ "module Foo"
    , "    ( -- group 1"
    , "      g1_0"
    , "    , g1_1"
    , "      -- group 2"
    , "    , g0_0"
    , "    , g0_1"
    , "    ) where"
    ]

ex12 :: Assertion
ex12 = assertSnippet (step Nothing defaultConfig {breakWhere = Always})
    [ "module Foo"
    , "  where"
    , "-- hmm"
    ]
    [ "module Foo"
    , "    where"
    , "-- hmm"
    ]

ex13 :: Assertion
ex13 = assertSnippet (step Nothing defaultConfig {breakWhere = Always, indent = 2})
    [ "module Foo where"
    ]
    [ "module Foo"
    , "  where"
    ]

ex14 :: Assertion
ex14 = assertSnippet (step Nothing defaultConfig {indent = 2})
    [ "module Foo"
    , "  ( yes"
    , "  , no"
    , "  ) where"
    ]
    [ "module Foo"
    , "  ( no"
    , "  , yes"
    , "  ) where"
    ]

ex15 :: Assertion
ex15 = assertSnippet (step Nothing defaultConfig {indent = 2})
    [ "module Foo -- Foo"
    , "("
    , " -- * t1 something"
    , "  t3,"
    , "  t1,"
    , " -- * t2 something"
    , "  t2"
    , ") where"
    ]
    [ "module Foo -- Foo"
    , "  ( -- * t1 something"
    , "    t1"
    , "  , t3"
    , "    -- * t2 something"
    , "  , t2"
    , "  ) where"
    ]

ex16 :: Assertion
ex16 = assertSnippet (step Nothing defaultConfig {sort = False}) input input
  where
    input =
      [ "module Foo"
      , "    ( yes"
      , "    , no"
      , "    ) where"
      ]

ex17 :: Assertion
ex17 = assertSnippet (step Nothing defaultConfig {separateLists = False})
    [ "module Foo"
    , "    ( Bar (..)"
    , "    ) where"
    ]
    [ "module Foo"
    , "    ( Bar(..)"
    , "    ) where"
    ]

ex18 :: Assertion
ex18 = assertSnippet (step Nothing defaultConfig {breakWhere = Always})
    [ "module Foo where"
    ]
    [ "module Foo"
    , "    where"
    ]

ex19 :: Assertion
ex19 = assertSnippet (step Nothing defaultConfig)
    [ "{-# LANGUAGE PatternSynonyms #-}"
    , "module Foo (Bar (.., Baz)) where"
    ]
    [ "{-# LANGUAGE PatternSynonyms #-}"
    , "module Foo"
    , "    ( Bar (.., Baz)"
    , "    ) where"
    ]

ex20 :: Assertion
ex20 = assertSnippet (step (Just 80) defaultConfig {breakWhere = Inline})
    [ "module Foo where"
    ]
    [ "module Foo where"
    ]

ex21 :: Assertion
ex21 = assertSnippet (step (Just 80) defaultConfig {breakWhere = Inline})
    [ "module Foo () where"
    ]
    [ "module Foo () where"
    ]

ex22 :: Assertion
ex22 = assertSnippet (step (Just 80) defaultConfig {breakWhere = Inline})
    [ "module Foo"
    , "    ( main"
    , "    ) where"
    ]
    [ "module Foo (main) where"
    ]

ex23 :: Assertion
ex23 = assertSnippet (step (Just 80) defaultConfig {breakWhere = Inline})
    [ "{-# LANGUAGE PatternSynonyms #-}"
    , "module Foo"
    , "    ( Foo(MkFoo)"
    , "    , Bar (.., Baz)"
    , "    ) where"
    ]
    [ "{-# LANGUAGE PatternSynonyms #-}"
    , "module Foo (Bar (.., Baz), Foo (MkFoo)) where"
    ]

ex24 :: Assertion
ex24 = assertSnippet (step (Just 80) defaultConfig {breakWhere = Inline})
    [ "module LongModuleName (longExportName1, longExportName2, longExportName3, longExportName4) where"
    ]
    [ "module LongModuleName"
    , "    ( longExportName1"
    , "    , longExportName2"
    , "    , longExportName3"
    , "    , longExportName4"
    , "    ) where"
    ]

ex25 :: Assertion
ex25 = assertSnippet (step Nothing defaultConfig {breakWhere = Inline})
    [ "module LongModuleName (longExportName1, longExportName2, longExportName3, longExportName4) where"
    ]
    [ "module LongModuleName (longExportName1, longExportName2, longExportName3, longExportName4) where"
    ]

ex26 :: Assertion
ex26 = assertSnippet (step (Just 80) defaultConfig {breakWhere = Inline})
    [ "module Foo ("
    , " -- * t1 something"
    , "  module A,"
    , "  t3,"
    , " -- * t2 something"
    , "  t2"
    , ") where -- x"
    , "-- y"
    ]
    [ "module Foo"
    , "    ( -- * t1 something"
    , "      module A"
    , "    , t3"
    , "      -- * t2 something"
    , "    , t2"
    , "    ) where -- x"
    , "-- y"
    ]

ex27 :: Assertion
ex27 = assertSnippet (step Nothing defaultConfig {breakWhere = Single})
    [ "module Foo where"
    ]
    [ "module Foo where"
    ]

ex28 :: Assertion
ex28 = assertSnippet (step Nothing defaultConfig {breakWhere = Single})
    [ "module Foo () where"
    ]
    [ "module Foo () where"
    ]

ex29 :: Assertion
ex29 = assertSnippet (step Nothing defaultConfig {breakWhere = Single})
    [ "module Foo"
    , "    ( main"
    , "    ) where"
    ]
    [ "module Foo (main) where"
    ]

ex30 :: Assertion
ex30 = assertSnippet (step Nothing defaultConfig {breakWhere = Single})
    [ "module Foo"
    , "    ( bar"
    , "    , foo"
    , "    ) where"
    ]
    [ "module Foo"
    , "    ( bar"
    , "    , foo"
    , "    ) where"
    ]

ex31 :: Assertion
ex31 = assertSnippet (step Nothing defaultConfig {breakWhere = Single})
    [ "module Foo"
    , "    ( -- * Foo"
    , "      Foo"
    , "    ) where"
    ]
    [ "module Foo"
    , "    ( -- * Foo"
    , "      Foo"
    , "    ) where"
    ]
