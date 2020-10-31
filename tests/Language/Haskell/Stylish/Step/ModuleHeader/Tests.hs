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
    , testCase "'where' not repeated when not part of exports with break_only_where" ex12
    , testCase "Indents absent export list with 2 spaces with break_only_where" ex13
    , testCase "Indents with 2 spaces" ex14
    , testCase "Group doc with 2 spaces" ex15
    , testCase "Does not sort" ex16
    , testCase "Repects separate_lists" ex17
    , testCase "Indents absent export list with break_only_where" ex18
    , testCase "Respects bundled patterns" ex19
    ]

--------------------------------------------------------------------------------
ex0 :: Assertion
ex0 = assertSnippet (step defaultConfig) input input
  where
    input =
      [ "module Foo where"
      ]

ex1 :: Assertion
ex1 = assertSnippet (step defaultConfig)
    [ "module Foo () where"
    ]
    [ "module Foo"
    , "    ("
    , "    ) where"
    ]

ex2 :: Assertion
ex2 = assertSnippet (step defaultConfig)
    [ "module Foo (tests) where"
    ]
    [ "module Foo"
    , "    ( tests"
    , "    ) where"
    ]

ex3 :: Assertion
ex3 = assertSnippet (step defaultConfig)
    [ "module Foo (t1, t2, t3) where"
    ]
    [ "module Foo"
    , "    ( t1"
    , "    , t2"
    , "    , t3"
    , "    ) where"
    ]

ex4 :: Assertion
ex4 = assertSnippet (step defaultConfig)
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
ex5 = assertSnippet (step defaultConfig)
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
ex6 = assertSnippet (step defaultConfig)
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
ex7 = assertSnippet (step defaultConfig)
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
ex8 = assertSnippet (step defaultConfig)
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
ex9 = assertSnippet (step defaultConfig)
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
ex10 = assertSnippet (step defaultConfig)
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
ex11 = assertSnippet (step defaultConfig)
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
ex12 = assertSnippet (step defaultConfig {breakOnlyWhere = True})
    [ "module Foo"
    , "  where"
    , "-- hmm"
    ]
    [ "module Foo"
    , "    where"
    , "-- hmm"
    ]

ex13 :: Assertion
ex13 = assertSnippet (step defaultConfig {breakOnlyWhere = True, indent = 2})
    [ "module Foo where"
    ]
    [ "module Foo"
    , "  where"
    ]

ex14 :: Assertion
ex14 = assertSnippet (step defaultConfig {indent = 2})
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
ex15 = assertSnippet (step defaultConfig {indent = 2})
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
ex16 = assertSnippet (step defaultConfig {sort = False}) input input
  where
    input =
      [ "module Foo"
      , "    ( yes"
      , "    , no"
      , "    ) where"
      ]

ex17 :: Assertion
ex17 = assertSnippet (step defaultConfig {separateLists = False})
    [ "module Foo"
    , "    ( Bar (..)"
    , "    ) where"
    ]
    [ "module Foo"
    , "    ( Bar(..)"
    , "    ) where"
    ]

ex18 :: Assertion
ex18 = assertSnippet (step defaultConfig {breakOnlyWhere = True})
    [ "module Foo where"
    ]
    [ "module Foo"
    , "    where"
    ]

ex19 :: Assertion
ex19 = assertSnippet (step defaultConfig)
    [ "{-# LANGUAGE PatternSynonyms #-}"
    , "module Foo (Bar (.., Baz)) where"
    ]
    [ "{-# LANGUAGE PatternSynonyms #-}"
    , "module Foo"
    , "    ( Bar (.., Baz)"
    , "    ) where"
    ]
