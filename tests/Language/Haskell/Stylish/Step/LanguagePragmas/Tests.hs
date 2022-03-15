--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists #-}
module Language.Haskell.Stylish.Step.LanguagePragmas.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import Test.Framework                 (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit                     (Assertion)


--------------------------------------------------------------------------------
import Language.Haskell.Stylish.Step.LanguagePragmas
import Language.Haskell.Stylish.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Language.Haskell.Stylish.Step.LanguagePragmas.Tests"
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

lANG :: String
lANG = "LANGUAGE"

--------------------------------------------------------------------------------
case01 :: Assertion
case01 = assertSnippet
    (step (Just 80) Vertical True False lANG)
    [ "{-# LANGUAGE ViewPatterns #-}"
    , "{-# LANGUAGE TemplateHaskell, ViewPatterns #-}"
    , "{-# LANGUAGE ScopedTypeVariables #-}"
    , "module Main where"
    ]

    [ "{-# LANGUAGE ScopedTypeVariables #-}"
    , "{-# LANGUAGE TemplateHaskell     #-}"
    , "{-# LANGUAGE ViewPatterns        #-}"
    , "module Main where"
    ]


--------------------------------------------------------------------------------
case02 :: Assertion
case02 = assertSnippet
    (step (Just 80) Vertical True True lANG)
    [ "{-# LANGUAGE BangPatterns #-}"
    , "{-# LANGUAGE ViewPatterns #-}"
    , "increment ((+ 1) -> x) = x"
    ]

    [ "{-# LANGUAGE ViewPatterns #-}"
    , "increment ((+ 1) -> x) = x"
    ]


--------------------------------------------------------------------------------
case03 :: Assertion
case03 = assertSnippet
    (step (Just 80) Vertical True True lANG)
    [ "{-# LANGUAGE BangPatterns #-}"
    , "{-# LANGUAGE ViewPatterns #-}"
    , "increment x = case x of !_ -> x + 1"
    ]

    [ "{-# LANGUAGE BangPatterns #-}"
    , "increment x = case x of !_ -> x + 1"
    ]


--------------------------------------------------------------------------------
case04 :: Assertion
case04 = assertSnippet
    (step (Just 80) Compact True False lANG)
    [ "{-# LANGUAGE TypeOperators, StandaloneDeriving, DeriveDataTypeable,"
    , "    TemplateHaskell #-}"
    , "{-# LANGUAGE TemplateHaskell, ViewPatterns #-}"
    ]

    [ "{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, " ++
        "TemplateHaskell,"
    , "             TypeOperators, ViewPatterns #-}"
    ]


--------------------------------------------------------------------------------
case05 :: Assertion
case05 = assertSnippet
    (step (Just 80) Vertical True False lANG)
    [ "{-# LANGUAGE CPP #-}"
    , ""
    , "#if __GLASGOW_HASKELL__ >= 702"
    , "{-# LANGUAGE Trustworthy #-}"
    , "#endif"
    ]

    [ "{-# LANGUAGE CPP         #-}"
    , ""
    , "#if __GLASGOW_HASKELL__ >= 702"
    , "{-# LANGUAGE Trustworthy #-}"
    , "#endif"
    ]


--------------------------------------------------------------------------------
case06 :: Assertion
case06 = assertSnippet
    (step (Just 80) CompactLine True False lANG)
    [ "{-# LANGUAGE TypeOperators, StandaloneDeriving, DeriveDataTypeable,"
    , "    TemplateHaskell #-}"
    , "{-# LANGUAGE TemplateHaskell, ViewPatterns #-}"
    ]
    [ "{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, " ++
      "TemplateHaskell #-}"
    , "{-# LANGUAGE TypeOperators, ViewPatterns                             #-}"
    ]

--------------------------------------------------------------------------------
case07 :: Assertion
case07 = assertSnippet
    (step (Just 80) Vertical False False lANG)
    [ "{-# LANGUAGE ViewPatterns #-}"
    , "{-# LANGUAGE TemplateHaskell, ViewPatterns #-}"
    , "{-# LANGUAGE ScopedTypeVariables, NoImplicitPrelude #-}"
    , "module Main where"
    ]

    [ "{-# LANGUAGE NoImplicitPrelude #-}"
    , "{-# LANGUAGE ScopedTypeVariables #-}"
    , "{-# LANGUAGE TemplateHaskell #-}"
    , "{-# LANGUAGE ViewPatterns #-}"
    , "module Main where"
    ]


--------------------------------------------------------------------------------
case08 :: Assertion
case08 = assertSnippet
    (step (Just 80) CompactLine False False lANG)
    [ "{-# LANGUAGE TypeOperators, StandaloneDeriving, DeriveDataTypeable,"
    , "    TemplateHaskell #-}"
    , "{-# LANGUAGE TemplateHaskell, ViewPatterns #-}"
    ]
    [ "{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, " ++
      "TemplateHaskell #-}"
    , "{-# LANGUAGE TypeOperators, ViewPatterns #-}"
    ]


--------------------------------------------------------------------------------
case09 :: Assertion
case09 = assertSnippet
    (step (Just 80) Compact True False lANG)
    [ "{-# LANGUAGE DefaultSignatures, FlexibleInstances, LambdaCase, " ++
      "TypeApplications"
    , "             #-}"
    ]
    [ "{-# LANGUAGE DefaultSignatures, FlexibleInstances, LambdaCase,"
    , "             TypeApplications #-}"
    ]

--------------------------------------------------------------------------------
case10 :: Assertion
case10 = assertSnippet
    (step (Just 80) Compact True False lANG)
    [ "{-# LANGUAGE NondecreasingIndentation, ScopedTypeVariables,"
    , "             TypeApplications #-}"
    ]
    [ "{-# LANGUAGE NondecreasingIndentation, ScopedTypeVariables, " ++
      "TypeApplications #-}"
    ]

--------------------------------------------------------------------------------
case11 :: Assertion
case11 = assertSnippet
    (step (Just 80) Vertical False False "language")
    [ "{-# LANGUAGE ViewPatterns #-}"
    , "{-# LANGUAGE TemplateHaskell, ViewPatterns #-}"
    , "{-# LANGUAGE ScopedTypeVariables, NoImplicitPrelude #-}"
    , "module Main where"
    ]

    [ "{-# language NoImplicitPrelude #-}"
    , "{-# language ScopedTypeVariables #-}"
    , "{-# language TemplateHaskell #-}"
    , "{-# language ViewPatterns #-}"
    , "module Main where"
    ]


--------------------------------------------------------------------------------
case12 :: Assertion
case12 = assertSnippet
    (step Nothing Compact False False "language")
    [ "{-# LANGUAGE ViewPatterns, OverloadedStrings #-}"
    , "{-# LANGUAGE TemplateHaskell, ViewPatterns #-}"
    , "{-# LANGUAGE ScopedTypeVariables, NoImplicitPrelude #-}"
    , "module Main where"
    ]

    [ "{-# language NoImplicitPrelude, OverloadedStrings, ScopedTypeVariables, TemplateHaskell, ViewPatterns #-}"
    , "module Main where"
    ]


--------------------------------------------------------------------------------
case13 :: Assertion
case13 = assertSnippet
    (step Nothing Vertical True True lANG) input input
  where
    input =
        [ "{-# LANGUAGE BangPatterns  #-}"
        , "{-# LANGUAGE DeriveFunctor #-}"
        , "main = let !x = 1 + 1 in print x"
        ]

--------------------------------------------------------------------------------
case14 :: Assertion
case14 = assertSnippet (step Nothing VerticalCompact False False "language")
    [ "{-# LANGUAGE ViewPatterns, OverloadedStrings #-}"
    , "{-# LANGUAGE TemplateHaskell, ViewPatterns #-}"
    , "{-# LANGUAGE ScopedTypeVariables, NoImplicitPrelude #-}"
    , "module Main where"
    ]
    [ "{-# language"
    , "    NoImplicitPrelude"
    , "  , OverloadedStrings"
    , "  , ScopedTypeVariables"
    , "  , TemplateHaskell"
    , "  , ViewPatterns"
    , "  #-}"
    , "module Main where"
    ]
