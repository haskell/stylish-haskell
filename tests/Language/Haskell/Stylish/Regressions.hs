{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Stylish.Regressions
  ( tests
  ) where

import           Language.Haskell.Stylish.Step.Imports
import           Language.Haskell.Stylish.Tests.Util (assertSnippet)
import           Test.Framework                      (Test, testGroup)
import           Test.Framework.Providers.HUnit      (testCase)
import           Test.HUnit                          (Assertion)


tests :: Test
tests = testGroup "Language.Haskell.Stylish.Regressions"
    [ testCase "case 00 (issue #198)" case00
    ]

-- | Error parsing '(,) #198
--
-- See https://github.com/haskell/stylish-haskell/issues/198
case00 :: Assertion
case00 = assertSnippet (step (Just 80) $ importStepConfig Global) input input
  where
    input =
      [ "{-# LANGUAGE TemplateHaskell #-}"
      , ""
      , "import           Language.Haskell.TH.Syntax"
      , ""
      , "main = print $ showName '(,)"
      ]

importStepConfig :: ImportAlign -> Options
importStepConfig align = defaultOptions { importAlign = align }
