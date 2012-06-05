--------------------------------------------------------------------------------
module StylishHaskell.Step.LanguagePragmas.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     ((@=?))


--------------------------------------------------------------------------------
import           StylishHaskell.Step.LanguagePragmas
import           StylishHaskell.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "StylishHaskell.Step.LanguagePragmas.Tests"
    [ case01
    , case02
    , case03
    , case04
    ]


--------------------------------------------------------------------------------
case01 :: Test
case01 = testCase "case 01" $
    expected @=? testStep (step Vertical False) input
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
case02 :: Test
case02 = testCase "case 02" $
    expected @=? testStep (step Vertical True) input
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
case03 :: Test
case03 = testCase "case 03" $
    expected @=? testStep (step Vertical True) input
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
case04 :: Test
case04 = testCase "case 04" $
    expected @=? testStep (step Compact False) input
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
