{-# LANGUAGE OverloadedLists #-}
module Language.Haskell.Stylish.Step.ModuleHeader.Tests
  ( tests
  ) where

--------------------------------------------------------------------------------
import           Prelude                                    hiding (lines)
import           Data.Function((&))
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
    , testCase "Does not indent absent export list, open_bracket = same_line" ex0a
    , testCase "Empty exports list" ex1
    , testCase "Empty exports list , open_bracket = same_line" ex1a
    , testCase "Single exported variable" ex2
    , testCase "Single exported variable, open_bracket = same_line" ex2a
    , testCase "Multiple exported variables" ex3
    , testCase "Multiple exported variables, open_bracket = same_line" ex3a
    , testCase "Only reformats module header" ex4
    , testCase "Only reformats module header, open_bracket = same_line" ex4a
    , testCase "Leaving pragmas in place" ex5
    , testCase "Leaving pragmas in place, open_bracket = same_line" ex5a
    , testCase "Leaving pragmas in place variant" ex6
    , testCase "Leaving pragmas in place variant, open_bracket = same_line" ex6a
    , testCase "Leaving comments in place" ex7
    , testCase "Leaving comments in place, open_bracket = same_line" ex7a
    , testCase "Exports all" ex8
    , testCase "Exports all, open_bracket = same_line" ex8a
    , testCase "Exports module" ex9
    , testCase "Exports module, open_bracket = same_line" ex9a
    , testCase "Exports symbol" ex10
    , testCase "Exports symbol, open_bracket = same_line" ex10a
    , testCase "Respects groups" ex11
    , testCase "Respects groups, open_bracket = same_line" ex11a
    , testCase "'where' not repeated when not part of exports with always break_where" ex12
    , testCase "'where' not repeated when not part of exports with always break_where, open_bracket = same_line" ex12a
    , testCase "Indents absent export list with 2 spaces with always break_where" ex13
    , testCase "Indents absent export list with 2 spaces with always break_where, open_bracket = same_line" ex13a
    , testCase "Indents with 2 spaces" ex14
    , testCase "Indents with 2 spaces, open_bracket = same_line" ex14a
    , testCase "Group doc with 2 spaces" ex15
    , testCase "Group doc with 2 spaces, open_bracket = same_line" ex15a
    , testCase "Does not sort" ex16
    , testCase "Repects separate_lists" ex17
    , testCase "Repects separate_lists, open_bracket = same_line" ex17a
    , testCase "Indents absent export list with always break_where" ex18
    , testCase "Respects bundled patterns" ex19
    , testCase "Respects bundled patterns, open_bracket = same_line" ex19a
    , testCase "Inline no export list" ex20
    , testCase "Inline empty export list" ex21
    , testCase "Inline empty export list, open_bracket = same_line" ex21a
    , testCase "Inline single export" ex22
    , testCase "Inline single export, open_bracket = same_line" ex22a
    , testCase "Inline single line sorts" ex23
    , testCase "Inline single line sorts, open_bracket = same_line" ex23a
    , testCase "Inline breaks when too long" ex24
    , testCase "Inline breaks when too long, open_bracket = same_line" ex24a
    , testCase "Inline single line when no max cols" ex25
    , testCase "Inline single line when no max cols, open_bracket = same_line" ex25a
    , testCase "Inline breaks when comments present" ex26
    , testCase "Inline breaks when comments present, open_bracket = same_line" ex26a
    , testCase "Single no export list" ex27
    , testCase "Single no export list, open_bracket = same_line" ex27a
    , testCase "Single empty export list" ex28
    , testCase "Single empty export list, open_bracket = same_line" ex28a
    , testCase "Single one export" ex29
    , testCase "Single one export, open_bracket = same_line" ex29a
    , testCase "Single two exports" ex30
    , testCase "Single two exports, open_bracket = same_line" ex30a
    , testCase "Single one export with comment" ex31
    , testCase "Single one export with comment, open_bracket = same_line" ex31a
    , testCase "Single one module comment" ex32
    , testCase "Inline comments" ex33
    ]

--------------------------------------------------------------------------------
ex0 :: Assertion
ex0 = assertSnippet (step Nothing defaultConfig) input input
  where
    input =
      [ "module Foo where"
      ]

ex0a :: Assertion
ex0a = assertSnippet (step Nothing $ defaultConfig & openBracketSameLine) input input
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

ex1a :: Assertion
ex1a = assertSnippet (step Nothing $ defaultConfig & openBracketSameLine)
    [ "module Foo () where"
    ]
    [ "module Foo ("
    , "     "
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

ex2a :: Assertion
ex2a = assertSnippet (step Nothing $ defaultConfig & openBracketSameLine)
    [ "module Foo (tests) where"
    ]
    [ "module Foo ("
    , "      tests"
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

ex3a :: Assertion
ex3a = assertSnippet (step Nothing $ defaultConfig & openBracketSameLine)
    [ "module Foo (t1, t2, t3) where"
    ]
    [ "module Foo ("
    , "      t1"
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

ex4a :: Assertion
ex4a = assertSnippet (step Nothing $ defaultConfig & openBracketSameLine)
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
    [ "module Foo ("
    , "      t1"
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

ex5a :: Assertion
ex5a = assertSnippet (step Nothing $ defaultConfig & openBracketSameLine)
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
    , "module Foo ("
    , "      t1"
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

ex6a :: Assertion
ex6a = assertSnippet (step Nothing $ defaultConfig & openBracketSameLine)
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
    , "module Foo ("
    , "      t1"
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
ex7a :: Assertion
ex7a = assertSnippet (step Nothing $ defaultConfig & openBracketSameLine)
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
    [ "module Foo ( -- Foo"
    , "      -- * t1 something"
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

ex8a :: Assertion
ex8a = assertSnippet (step Nothing $ defaultConfig & openBracketSameLine)
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
    [ "module Foo ("
    , "      -- * t1 something"
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

ex9a :: Assertion
ex9a = assertSnippet (step Nothing $ defaultConfig & openBracketSameLine)
    [ "module Foo ("
    , " -- * t1 something"
    , "  module A,"
    , "  t3,"
    , " -- * t2 something"
    , "  t2"
    , ") where -- x"
    , "-- y"
    ]
    [ "module Foo ("
    , "      -- * t1 something"
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

ex10a :: Assertion
ex10a = assertSnippet (step Nothing $ defaultConfig & openBracketSameLine)
    [ "module Foo ("
    , "  (<&>)"
    , ") where -- x"
    , "-- y"
    ]
    [ "module Foo ("
    , "      (<&>)"
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

ex11a :: Assertion
ex11a = assertSnippet (step Nothing $ defaultConfig & openBracketSameLine)
    [ "module Foo ("
    , "  -- group 1"
    , " g1_1,"
    , " g1_0,"
    , "  -- group 2"
    , " g0_1,"
    , " g0_0"
    , ") where"
    ]
    [ "module Foo ("
    , "      -- group 1"
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
ex12a :: Assertion
ex12a = assertSnippet (step Nothing $ defaultConfig {breakWhere = Always} & openBracketSameLine)
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

ex13a :: Assertion
ex13a = assertSnippet (step Nothing $ defaultConfig {breakWhere = Always, indent = 2} & openBracketSameLine)
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

ex14a :: Assertion
ex14a = assertSnippet (step Nothing $ defaultConfig {indent = 2} & openBracketSameLine)
    [ "module Foo"
    , "  ( yes"
    , "  , no"
    , "  ) where"
    ]
    [ "module Foo ("
    , "    no"
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

ex15a :: Assertion
ex15a = assertSnippet (step Nothing $ defaultConfig {indent = 2} & openBracketSameLine)
    [ "module Foo -- Foo"
    , "("
    , " -- * t1 something"
    , "  t3,"
    , "  t1,"
    , " -- * t2 something"
    , "  t2"
    , ") where"
    ]
    [ "module Foo ( -- Foo"
    , "    -- * t1 something"
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

ex17a :: Assertion
ex17a = assertSnippet (step Nothing $ defaultConfig {separateLists = False} & openBracketSameLine)
    [ "module Foo"
    , "    ( Bar (..)"
    , "    ) where"
    ]
    [ "module Foo ("
    , "      Bar(..)"
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

ex19a :: Assertion
ex19a = assertSnippet (step Nothing $ defaultConfig & openBracketSameLine)
    [ "{-# LANGUAGE PatternSynonyms #-}"
    , "module Foo (Bar (.., Baz)) where"
    ]
    [ "{-# LANGUAGE PatternSynonyms #-}"
    , "module Foo ("
    , "      Bar (.., Baz)"
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

ex21a :: Assertion
ex21a = assertSnippet (step (Just 80) $ defaultConfig {breakWhere = Inline} & openBracketSameLine)
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

ex22a :: Assertion
ex22a = assertSnippet (step (Just 80) $ defaultConfig {breakWhere = Inline} & openBracketSameLine)
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

ex23a  :: Assertion
ex23a = assertSnippet (step (Just 80) $ defaultConfig {breakWhere = Inline} & openBracketSameLine)
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

ex24a :: Assertion
ex24a = assertSnippet (step (Just 80) $ defaultConfig {breakWhere = Inline} & openBracketSameLine)
    [ "module LongModuleName (longExportName1, longExportName2, longExportName3, longExportName4) where"
    ]
    [ "module LongModuleName ("
    , "      longExportName1"
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

ex25a :: Assertion
ex25a = assertSnippet (step Nothing $ defaultConfig {breakWhere = Inline} & openBracketSameLine)
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

ex26a :: Assertion
ex26a = assertSnippet (step (Just 80) $ defaultConfig {breakWhere = Inline} & openBracketSameLine)
    [ "module Foo ("
    , " -- * t1 something"
    , "  module A,"
    , "  t3,"
    , " -- * t2 something"
    , "  t2"
    , ") where -- x"
    , "-- y"
    ]
    [ "module Foo ("
    , "      -- * t1 something"
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

ex27a :: Assertion
ex27a = assertSnippet (step Nothing $ defaultConfig {breakWhere = Single} & openBracketSameLine)
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

ex28a :: Assertion
ex28a = assertSnippet (step Nothing $ defaultConfig {breakWhere = Single} & openBracketSameLine)
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

ex29a :: Assertion
ex29a = assertSnippet (step Nothing $ defaultConfig {breakWhere = Single} & openBracketSameLine)
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

ex30a :: Assertion
ex30a = assertSnippet (step Nothing $ defaultConfig {breakWhere = Single} & openBracketSameLine)
    [ "module Foo"
    , "    ( bar"
    , "    , foo"
    , "    ) where"
    ]
    [ "module Foo ("
    , "      bar"
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

ex31a :: Assertion
ex31a = assertSnippet (step Nothing $ defaultConfig {breakWhere = Single} & openBracketSameLine)
    [ "module Foo"
    , "    ( -- * Foo"
    , "      Foo"
    , "    ) where"
    ]
    [ "module Foo ("
    , "      -- * Foo"
    , "      Foo"
    , "    ) where"
    ]

ex32 :: Assertion
ex32 = assertSnippet (step Nothing $ defaultConfig {breakWhere = Single})
    [ "module Foo (bar) where -- Foo"
    ]
    [ "module Foo (bar) where -- Foo"
    ]

ex33 :: Assertion
ex33 = assertSnippet (step Nothing $ defaultConfig)
    [ "module Foo ("
    , "  -- Bar"
    , "  bar, -- Inline bar"
    , "  -- Foo"
    , "  foo -- Inline foo"
    , ") where"
    ]
    [ "module Foo"
    , "    ( -- Bar"
    , "      bar -- Inline bar"
    , "      -- Foo"  -- NOTE(jaspervdj): I would prefer to have the `,` here
    , "    , foo -- Inline foo"
    , "    ) where"
    ]

openBracketSameLine :: Config -> Config
openBracketSameLine cfg = cfg { openBracket = SameLine }
