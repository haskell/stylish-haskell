{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Stylish.Regressions
  ( tests
  ) where

import qualified System.IO                           as IO (Newline (..))

import           Language.Haskell.Stylish            (formatWith)
import           Language.Haskell.Stylish.Config     (Config (..),
                                                      ExitCodeBehavior (..))
import qualified Language.Haskell.Stylish.Step.Data  as Data
import           Language.Haskell.Stylish.Step.Imports
import qualified Language.Haskell.Stylish.Step.SimpleAlign as SimpleAlign
import qualified Language.Haskell.Stylish.Step.TrailingWhitespace as TrailingWhitespace
import           Language.Haskell.Stylish.Tests.Util (assertSnippet)
import           Test.Framework                      (Test, testGroup)
import           Test.Framework.Providers.HUnit      (testCase)
import           Test.HUnit                          (Assertion, (@?=))


tests :: Test
tests = testGroup "Language.Haskell.Stylish.Regressions"
    [ testCase "case 00 (issue #198)" case00
    , testCase "case 01 (embedded newlines in Printer)" case01
    , testCase "case 02 (deriving via with gap strings)" case02
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


--------------------------------------------------------------------------------
regressionConf :: Config
regressionConf = Config
    { configSteps =
        [ Data.step Data.Config
            { Data.cEquals                  = Data.Indent 2
            , Data.cFirstField              = Data.Indent 2
            , Data.cFieldComment            = 2
            , Data.cDeriving                = 2
            , Data.cBreakEnums              = False
            , Data.cBreakSingleConstructors = True
            , Data.cVia                     = Data.Indent 2
            , Data.cCurriedContext          = False
            , Data.cSortDeriving            = True
            , Data.cMaxColumns              = Data.MaxColumns 80
            }
        , SimpleAlign.step (Just 80) SimpleAlign.Config
            { SimpleAlign.cCases            = SimpleAlign.Always
            , SimpleAlign.cTopLevelPatterns = SimpleAlign.Always
            , SimpleAlign.cRecords          = SimpleAlign.Always
            , SimpleAlign.cMultiWayIf       = SimpleAlign.Always
            }
        , TrailingWhitespace.step
        ]
    , configColumns            = Just 80
    , configLanguageExtensions =
        ["BangPatterns", "DerivingVia", "DataKinds", "TypeOperators", "OverloadedStrings"]
    , configNewline            = IO.LF
    , configCabal              = False
    , configExitCode           = NormalExitBehavior
    }


-- | When the Data step produces lines with embedded newlines (from
-- showOutputable wrapping long types), subsequent steps like simple_align
-- would miscount lines and corrupt identifiers.
case01 :: Assertion
case01 = actual @?= Right expected
  where
    actual = formatWith regressionConf Nothing (unlines input)
    input =
        [ "module Foo where"
        , ""
        , "data FooRec"
        , "  = FooRec"
        , "      { fooBarMap       :: !(M.Map Name Xid)"
        , "      , bazQuxLookupMap :: !(M.Map (Name, SomeLongKeyType, AnotherVeryLongKeyName) SomeVeryLongResultType)"
        , "      , fooItems        :: ![Xs]"
        , "      , fooBatches      :: ![Ys]"
        , "      }"
        , ""
        , "data BarResult"
        , "  = BarNothing"
        , "      { brNeErrors :: NonEmpty AppError"
        , "      }"
        , "  | BarJust"
        , "      { brResult   :: ([Dp], [(Gcn, [Cn])])"
        , "      , brErrors   :: [AppError]"
        , "      , brWarnings :: [AppWarning]"
        , "      , brXs :: [Xs]"
        , "      , brYs  :: [Ys]"
        , "      }"
        ]
    expected =
        [ "module Foo where"
        , ""
        , "data FooRec"
        , "  = FooRec"
        , "      { fooBarMap :: !(M.Map Name Xid)"
        , "      , bazQuxLookupMap :: !(M.Map (Name, SomeLongKeyType, AnotherVeryLongKeyName) SomeVeryLongResultType)"
        , "      , fooItems :: ![Xs]"
        , "      , fooBatches :: ![Ys]"
        , "      }"
        , ""
        , "data BarResult"
        , "  = BarNothing"
        , "      { brNeErrors :: NonEmpty AppError"
        , "      }"
        , "  | BarJust"
        , "      { brResult   :: ([Dp], [(Gcn, [Cn])])"
        , "      , brErrors   :: [AppError]"
        , "      , brWarnings :: [AppWarning]"
        , "      , brXs       :: [Xs]"
        , "      , brYs       :: [Ys]"
        , "      }"
        ]


-- | When the Data step processes a deriving via clause containing a gap string
-- (multi-line string literal using \<newline><spaces>\ syntax), the embedded
-- newlines should be properly split across Lines entries so subsequent steps
-- don't miscount lines and corrupt identifiers.
case02 :: Assertion
case02 = actual @?= Right expected
  where
    actual = formatWith regressionConf Nothing (unlines input)
    input =
        [ "module Foo where"
        , ""
        , "data MyRecord"
        , "  = MyRecord"
        , "      { mrName  :: !Name"
        , "      , mrItems :: !(NonEmpty MyItem)"
        , "      }"
        , "  deriving (Eq, Show, Ord, Generic)"
        , "  deriving (ToJSON, FromJSON, ToSchema) via CustomEncoding (Wrapper MyRecord"
        , "    '[ \"mrName\" `With` WithLabel \"label\""
        , "     , \"mrItems\" `With` WithLabel"
        , "         \"Some long text.\\"
        , "         \\ Split across multiple \\"
        , "         \\ lines\""
        , "     ])"
        , ""
        , "data MyItem"
        , "  = MyItem"
        , "      { miEntries      :: !(NonEmpty Rf)"
        , "      , miEnabled      :: !Bool"
        , "      , miLookupResult :: !(Maybe (Abc Def))"
        , "      }"
        , "  deriving (Eq, Show, Ord, Generic)"
        ]
    expected =
        [ "module Foo where"
        , ""
        , "data MyRecord"
        , "  = MyRecord"
        , "      { mrName  :: !Name"
        , "      , mrItems :: !(NonEmpty MyItem)"
        , "      }"
        , "  deriving (Eq, Generic, Ord, Show)"
        , "  deriving (FromJSON, ToJSON, ToSchema)"
        , "    via CustomEncoding (Wrapper MyRecord '[\"mrName\" `With` WithLabel \"label\", \"mrItems\" `With` WithLabel \"Some long text.\\"
        , "         \\ Split across multiple \\"
        , "         \\ lines\"])"
        , ""
        , "data MyItem"
        , "  = MyItem"
        , "      { miEntries      :: !(NonEmpty Rf)"
        , "      , miEnabled      :: !Bool"
        , "      , miLookupResult :: !(Maybe (Abc Def))"
        , "      }"
        , "  deriving (Eq, Generic, Ord, Show)"
        ]
