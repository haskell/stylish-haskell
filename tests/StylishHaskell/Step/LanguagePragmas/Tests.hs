--------------------------------------------------------------------------------
module StylishHaskell.Step.LanguagePragmas.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                      (Test, testGroup)
import           Test.Framework.Providers.HUnit      (testCase)
import           Test.HUnit                          (Assertion, (@=?))


--------------------------------------------------------------------------------
import           StylishHaskell.Step.LanguagePragmas
import           StylishHaskell.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "StylishHaskell.Step.LanguagePragmas.Tests"
    [ testCase "case 01" case01
    , testCase "case 02" case02
    , testCase "case 03" case03
    , testCase "case 04" case04
    ]


--------------------------------------------------------------------------------
case01 :: Assertion
case01 = expected @=? testStep (step Vertical False) input
  where
    input = unlines
        [ "{-# LANGUAGE ViewPatterns #-}"
        , "{-# LANGUAGE TemplateHaskell, ViewPatterns #-}"
        , "{-# LANGUAGE ScopedTypeVariables #-}"
        , "module Main where"
        ]

    expected = unlines
        [ "{-# LANGUAGE ScopedTypeVariables #-}"
        , "{-# LANGUAGE TemplateHaskell     #-}"
        , "{-# LANGUAGE ViewPatterns        #-}"
        , "module Main where"
        ]


--------------------------------------------------------------------------------
case02 :: Assertion
case02 = expected @=? testStep (step Vertical True) input
  where
    input = unlines
        [ "{-# LANGUAGE BangPatterns #-}"
        , "{-# LANGUAGE ViewPatterns #-}"
        , "increment ((+ 1) -> x) = x"
        ]

    expected = unlines
        [ "{-# LANGUAGE ViewPatterns #-}"
        , "increment ((+ 1) -> x) = x"
        ]


--------------------------------------------------------------------------------
case03 :: Assertion
case03 = expected @=? testStep (step Vertical True) input
  where
    input = unlines
        [ "{-# LANGUAGE BangPatterns #-}"
        , "{-# LANGUAGE ViewPatterns #-}"
        , "increment x = case x of !_ -> x + 1"
        ]

    expected = unlines
        [ "{-# LANGUAGE BangPatterns #-}"
        , "increment x = case x of !_ -> x + 1"
        ]


--------------------------------------------------------------------------------
case04 :: Assertion
case04 = expected @=? testStep (step Compact False) input
  where
    input = unlines
        [ "{-# LANGUAGE TypeOperators, StandaloneDeriving, DeriveDataTypeable,"
        , "    TemplateHaskell #-}"
        , "{-# LANGUAGE TemplateHaskell, ViewPatterns #-}"
        ]

    expected = unlines
        [ "{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, " ++
            "TemplateHaskell,"
        , "             TypeOperators, ViewPatterns #-}"
        ]
