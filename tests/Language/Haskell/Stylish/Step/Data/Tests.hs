module Language.Haskell.Stylish.Step.Data.Tests
    ( tests
    ) where

import           Language.Haskell.Stylish.Step.Data
import           Language.Haskell.Stylish.Tests.Util (testStep)
import           Test.Framework                      (Test, testGroup)
import           Test.Framework.Providers.HUnit      (testCase)
import           Test.HUnit                          (Assertion, (@=?))

tests :: Test
tests = testGroup "Language.Haskell.Stylish.Step.Data.Tests"
    [ testCase "case 00" case00
    , testCase "case 01" case01
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
    , testCase "case 15" case15
    , testCase "case 16" case16
    , testCase "case 17" case17
    , testCase "case 18" case18
    , testCase "case 19" case19
    , testCase "case 20 (issue 262)" case20
    , testCase "case 21" case21
    , testCase "case 22" case22
    , testCase "case 23" case23
    , testCase "case 24" case24
    , testCase "case 25" case25
    , testCase "case 26" case26
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
    ]

case00 :: Assertion
case00 = expected @=? testStep (step sameSameStyle) input
  where
    input = unlines
      [ "module Herp where"
      , ""
      , "data Foo"
      ]

    expected = input

case01 :: Assertion
case01 = expected @=? testStep (step indentIndentStyle) input
  where
    input = unlines
      [ "module Herp where"
      , ""
      , "data Foo = Foo { a :: Int }"
      ]

    expected = unlines
       [ "module Herp where"
       , ""
       , "data Foo"
       , "  = Foo"
       , "      { a :: Int"
       , "      }"
       ]

case02 :: Assertion
case02 = expected @=? testStep (step indentIndentStyle) input
  where
    input = unlines
      [ "module Herp where"
      , ""
      , "data Foo = Foo { a :: Int, a2 :: String }"
      ]
    expected = unlines
       [ "module Herp where"
       , ""
       , "data Foo"
       , "  = Foo"
       , "      { a :: Int"
       , "      , a2 :: String"
       , "      }"
       ]

case03 :: Assertion
case03 = expected @=? testStep (step indentIndentStyle) input
  where
    input = unlines
      [ "module Herp where"
      , ""
      , "data Foo a = Foo { a :: a, a2 :: String }"
      ]
    expected = unlines
       [ "module Herp where"
       , ""
       , "data Foo a"
       , "  = Foo"
       , "      { a :: a"
       , "      , a2 :: String"
       , "      }"
       ]

case04 :: Assertion
case04 = expected @=? testStep (step indentIndentStyle) input
  where
    input = unlines
      [ "module Herp where"
      , ""
      , "data Foo a = Foo { a :: a, a2 :: String } | Bar { b :: a }"
      ]
    expected = unlines
       [ "module Herp where"
       , ""
       , "data Foo a"
       , "  = Foo"
       , "      { a :: a"
       , "      , a2 :: String"
       , "      }"
       , "  | Bar"
       , "      { b :: a"
       , "      }"
       ]

case05 :: Assertion
case05 = expected @=? testStep (step indentIndentStyle) input
  where
    input = unlines
       [ "module Herp where"
       , ""
       , "data Foo = Foo {"
       , "  a :: Int"
       , "  , a2 :: String"
       , "  }"
       ]
    expected = unlines
       [ "module Herp where"
       , ""
       , "data Foo"
       , "  = Foo"
       , "      { a :: Int"
       , "      , a2 :: String"
       , "      }"
       ]

case06 :: Assertion
case06 = expected @=? testStep (step sameSameStyle) input
  where
    input = unlines
      [ "module Herp where"
      , ""
      , "data Foo = Foo Int String"
      ]
    expected = input

case07 :: Assertion
case07 = expected @=? testStep (step sameSameStyle) input
  where
    input = unlines
      [ "module Herp where"
      , ""
      , "data Phantom a = Phantom"
      ]
    expected = input

case08 :: Assertion
case08 = expected @=? testStep (step sameSameStyle) input
  where
    input = unlines
      [ "module Herp where"
      , ""
      , "data Phantom a ="
      , "  Phantom"
      ]
    expected = unlines
      [ "module Herp where"
      , ""
      , "data Phantom a = Phantom"
      ]

case09 :: Assertion
case09 = expected @=? testStep (step indentIndentStyle4) input
  where
    input = unlines
      [ "module Herp where"
      , ""
      , "data Foo a b = Foo { a :: a, a2 :: String } | Bar { b :: a, c:: b }"
      ]
    expected = unlines
       [ "module Herp where"
       , ""
       , "data Foo a b"
       , "    = Foo"
       , "          { a :: a"
       , "          , a2 :: String"
       , "          }"
       , "    | Bar"
       , "          { b :: a"
       , "          , c :: b"
       , "          }"
       ]

case10 :: Assertion
case10 = expected @=? testStep (step indentIndentStyle) input
  where
    input = unlines
      [ "module Herp where"
      , ""
      , "data Foo = Foo { a :: Int } deriving (Eq, Generic) deriving (Show)"
      ]

    expected = unlines
       [ "module Herp where"
       , ""
       , "data Foo"
       , "  = Foo"
       , "      { a :: Int"
       , "      }"
       , "  deriving (Eq, Generic)"
       , "  deriving (Show)"
       ]

case11 :: Assertion
case11 = expected @=? testStep (step indentIndentStyle) input
  where
    input = unlines
      [ "{-# LANGUAGE DerivingStrategies #-}"
      , "module Herp where"
      , ""
      , "data Foo = Foo { a :: Int } deriving stock (Show)"
      ]

    expected = unlines
       [ "{-# LANGUAGE DerivingStrategies #-}"
       , "module Herp where"
       , ""
       , "data Foo"
       , "  = Foo"
       , "      { a :: Int"
       , "      }"
       , "  deriving stock (Show)"
       ]

case12 :: Assertion
case12 = expected @=? testStep (step indentIndentStyle4) input
  where
    input = unlines
      [ "module Herp where"
      , ""
      , "data Point = Point { pointX, pointY :: Double , pointName :: String} deriving (Show)"
      ]

    expected = unlines
       [ "module Herp where"
       , ""
       , "data Point"
       , "    = Point"
       , "          { pointX, pointY :: Double"
       , "          , pointName :: String"
       , "          }"
       , "    deriving (Show)"
       ]

case13 :: Assertion
case13 = expected @=? testStep (step indentIndentStyle) input
  where
    input = unlines
      [ "module Herp where"
      , ""
      , "-- this is a comment"
      , "data Foo = Foo { a :: Int }"
      ]
    expected = unlines
      [ "module Herp where"
      , ""
      , "-- this is a comment"
      , "data Foo"
      , "  = Foo"
      , "      { a :: Int"
      , "      }"
      ]

case14 :: Assertion
case14 = expected @=? testStep (step indentIndentStyle) input
  where
    input = unlines
      [ "module Herp where"
      , ""
      , "{- this is"
      , "   a comment -}"
      , "data Foo = Foo { a :: Int }"
      ]
    expected = unlines
      [ "module Herp where"
      , ""
      , "{- this is"
      , "   a comment -}"
      , "data Foo"
      , "  = Foo"
      , "      { a :: Int"
      , "      }"
      ]

case15 :: Assertion
case15 = expected @=? testStep (step indentIndentStyle) input
  where
    input = unlines
       [ "module Herp where"
       , ""
       , "data Foo a = Foo"
       , "  { a :: a, -- comment"
       , "   a2 :: String"
       , "  }"
       ]
    expected = unlines
       [ "module Herp where"
       , ""
       , "data Foo a"
       , "  = Foo"
       , "      { a :: a -- comment"
       , "      , a2 :: String"
       , "      }"
       ]

case16 :: Assertion
case16 = expected @=? testStep (step indentIndentStyle) input
  where
    input = unlines
      [ "module Herp where"
      , ""
      , "data Foo = Foo {"
      , "   a :: Int -- ^ comment"
      , "  }"
      ]
    expected = unlines
      [ "module Herp where"
      , ""
      , "data Foo"
      , "  = Foo"
      , "      { a :: Int"
      , "        -- ^ comment"
      , "      }"
      ]

case17 :: Assertion
case17 = expected @=? testStep (step indentIndentStyle) input
  where
    input = unlines
       [ "module Herp where"
       , ""
       , "data Foo a = Foo"
       , "  { a :: a,"
       , "-- comment"
       , "   a2 :: String"
       , "  }"
       ]
    expected = unlines
       [ "module Herp where"
       , ""
       , "data Foo a"
       , "  = Foo"
       , "      { a :: a"
       , "        -- comment"
       , "      , a2 :: String"
       , "      }"
       ]

case18 :: Assertion
case18 = expected @=? testStep (step indentIndentStyle) input
  where
    input = unlines
       [ "module Herp where"
       , ""
       , "data Foo a = Foo"
       , "  { a :: a,"
       , "-- ^ comment"
       , "   a2 :: String"
       , "  }"
       ]
    expected = unlines
       [ "module Herp where"
       , ""
       , "data Foo a"
       , "  = Foo"
       , "      { a :: a"
       , "        -- ^ comment"
       , "      , a2 :: String"
       , "      }"
       ]

case19 :: Assertion
case19 = expected @=? testStep (step indentIndentStyle) input
  where
    input = unlines
       [ "module Herp where"
       , ""
       , "data Foo a = Foo"
       , "  { firstName, lastName :: String,"
       , "-- ^ names"
       , "   age :: Int"
       , "  }"
       ]
    expected = unlines
       [ "module Herp where"
       , ""
       , "data Foo a"
       , "  = Foo"
       , "      { firstName, lastName :: String"
       , "        -- ^ names"
       , "      , age :: Int"
       , "      }"
       ]

-- | Should not break Enums (data without records) formatting
--
-- See https://github.com/jaspervdj/stylish-haskell/issues/262
case20 :: Assertion
case20 = input @=? testStep (step indentIndentStyle) input
  where
    input = unlines
       [ "module Herp where"
       , ""
       , "data Tag = Title | Text deriving (Eq, Show)"
       ]

case21 :: Assertion
case21 = expected @=? testStep (step sameSameStyle) input
  where
    input = unlines
       [ "data Foo a"
       , "  = Foo { a :: Int,"
       , "          a2 :: String"
       , "          -- ^ some haddock"
       , "        }"
       , "  | Bar { b :: a }  deriving (Eq, Show)"
       , "  deriving (ToJSON)"
       ]

    expected = unlines
       [ "data Foo a = Foo { a :: Int"
       , "                 , a2 :: String"
       , "                   -- ^ some haddock"
       , "                 }"
       , "           | Bar { b :: a"
       , "                 }"
       , "  deriving (Eq, Show)"
       , "  deriving (ToJSON)"
       ]

case22 :: Assertion
case22 = expected @=? testStep (step sameIndentStyle) input
  where
    input = unlines
       [ "data Foo a"
       , "  = Foo { a :: Int,"
       , "          a2 :: String"
       , "          -- ^ some haddock"
       , "        }"
       , "  | Bar { b :: a }  deriving (Eq, Show)"
       , "  deriving (ToJSON)"
       ]

    expected = unlines
       [ "data Foo a = Foo"
       , "               { a :: Int"
       , "               , a2 :: String"
       , "                 -- ^ some haddock"
       , "               }"
       , "           | Bar"
       , "               { b :: a"
       , "               }"
       , "  deriving (Eq, Show)"
       , "  deriving (ToJSON)"
       ]

case23 :: Assertion
case23 = expected @=? testStep (step indentSameStyle) input
  where
    input = unlines
       [ "data Foo a"
       , "  = Foo { a :: Int,"
       , "          a2 :: String"
       , "          -- ^ some haddock"
       , "        }"
       , "  | Bar { b :: a }  deriving (Eq, Show)"
       , "  deriving (ToJSON)"
       ]

    expected = unlines
       [ "data Foo a"
       , "  = Foo { a :: Int"
       , "        , a2 :: String"
       , "          -- ^ some haddock"
       , "        }"
       , "  | Bar { b :: a"
       , "        }"
       , "  deriving (Eq, Show)"
       , "  deriving (ToJSON)"
       ]

case24 :: Assertion
case24 = expected @=? testStep (step indentIndentStyle) input
  where
    input = unlines
       [ "data Foo a"
       , "  = Foo { a :: Int,"
       , "          a2 :: String"
       , "          -- ^ some haddock"
       , "        }"
       , "  | Bar { b :: a }  deriving (Eq, Show)"
       , "  deriving (ToJSON)"
       ]

    expected = unlines
       [ "data Foo a"
       , "  = Foo"
       , "      { a :: Int"
       , "      , a2 :: String"
       , "        -- ^ some haddock"
       , "      }"
       , "  | Bar"
       , "      { b :: a"
       , "      }"
       , "  deriving (Eq, Show)"
       , "  deriving (ToJSON)"
       ]

case25 :: Assertion
case25 = expected @=? testStep (step indentIndentStyle { cBreakSingleConstructors = False }) input
  where
    input = unlines
       [ "data Foo a"
       , "  = Foo { a :: Int,"
       , "          a2 :: String"
       , "          -- ^ some haddock"
       , "        }"
       , "  deriving (Eq, Show)"
       , "  deriving (ToJSON)"
       ]

    expected = unlines
       [ "data Foo a = Foo"
       , "  { a :: Int"
       , "  , a2 :: String"
       , "    -- ^ some haddock"
       , "  }"
       , "  deriving (Eq, Show)"
       , "  deriving (ToJSON)"
       ]

case26 :: Assertion
case26 = expected @=? testStep (step indentIndentStyle) input
  where
    input = unlines
      [ "module Herp where"
      , ""
      , "data Foo = Foo { a :: Int } deriving (FromJSON) via Bla Foo"
      ]

    expected = unlines
       [ "module Herp where"
       , ""
       , "data Foo"
       , "  = Foo"
       , "      { a :: Int"
       , "      }"
       , "  deriving (FromJSON) via Bla Foo"
       ]

case27 :: Assertion
case27 = expected @=? testStep (step sameIndentStyle { cBreakEnums = True }) input
  where
    input = unlines
      [ "module Herp where"
      , ""
      , "data Foo = Foo | Bar | Baz deriving (Eq, Show)"
      ]

    expected = unlines
      [ "module Herp where"
      , ""
      , "data Foo"
      , "  = Foo"
      , "  | Bar"
      , "  | Baz"
      , "  deriving (Eq, Show)"
      ]

case28 :: Assertion
case28 = expected @=? testStep (step sameIndentStyle { cBreakEnums = True }) input
  where
    input = unlines
      [ "module Some.Types where"
      , ""
      , "newtype BankCode = BankCode {"
      , "    unBankCode :: Text"
      , "  }"
      , "  deriving stock (Generic, Eq, Show)"
      , "  deriving anyclass (Newtype)"
      , ""
      , "newtype CheckDigit = CheckDigit { unCheckDigit :: Text }"
      , "  deriving stock (Generic, Eq, Show)"
      , "  deriving anyclass (Newtype)"
      , ""
      , "newtype WrappedInt = WrappedInt Int"
      , "  deriving stock (Generic, Eq, Show)"
      , "  deriving anyclass (Newtype)"
      , ""
      , "data MandateStatus"
      , "  = Approved"
      , "  | Failed"
      , "  | UserCanceled"
      , "  | Inactive"
      , "  deriving stock (Generic, Show, Eq, Enum, Bounded)"
      , "  deriving (ToJSON, FromJSON) via SnakeCaseCapsEnumEncoding MandateStatus"
      ]

    expected = unlines
      [ "module Some.Types where"
      , ""
      , "newtype BankCode = BankCode { unBankCode :: Text }"
      , "  deriving stock (Generic, Eq, Show)"
      , "  deriving anyclass (Newtype)"
      , ""
      , "newtype CheckDigit = CheckDigit { unCheckDigit :: Text }"
      , "  deriving stock (Generic, Eq, Show)"
      , "  deriving anyclass (Newtype)"
      , ""
      , "newtype WrappedInt = WrappedInt Int"
      , "  deriving stock (Generic, Eq, Show)"
      , "  deriving anyclass (Newtype)"
      , ""
      , "data MandateStatus"
      , "  = Approved"
      , "  | Failed"
      , "  | UserCanceled"
      , "  | Inactive"
      , "  deriving stock (Generic, Show, Eq, Enum, Bounded)"
      , "  deriving (ToJSON, FromJSON) via SnakeCaseCapsEnumEncoding MandateStatus"
      ]

case29 :: Assertion
case29 = expected @=? testStep (step sameIndentStyle) input
  where
    input = unlines
      [ "module Some.Types where"
      , ""
      , "data NonEmpty a"
      , "  = a :| [a]"
      ]

    expected = unlines
      [ "module Some.Types where"
      , ""
      , "data NonEmpty a = a :| [a]"
      ]

case30 :: Assertion
case30 = expected @=? testStep (step sameIndentStyle { cBreakEnums = True }) input
  where
    expected = input
    input = unlines
      [ "data ReasonCode"
      , "  = MissingTenantId"
      , "  -- Transaction errors:"
      , "  | TransactionDoesNotExist"
      , "  | TransactionAlreadyExists"
      , "  -- Engine errors:"
      , "  | EnginePersistenceError"
      , "  | EngineValidationError"
      , "  -- | Transaction was created in Info mode"
      , "  | RegisteredByNetworkEngine"
      , "  -- | Transaction was created in Routing mode"
      , "  | SentToNetworkEngine"
      , "  -- Network connection reasons:"
      , "  | SentToNetworkConnection"
      , "  | ReceivedByNetworkConnection"
      , "  | ValidatedByNetworkConnection"
      ]


case31 :: Assertion
case31 = expected @=? testStep (step indentIndentStyle { cBreakEnums = True }) input
  where
    expected = input
    input = unlines
      [ "data ConfiguredLogger"
      , "  -- | Logs to file"
      , "  = LogTo FilePath"
      , "  -- | Logs to stdout"
      , "  | LogToConsole"
      , "  -- | No logging, discards all messages"
      , "  | NoLogging"
      , "  deriving stock (Generic, Show)"
      ]

case32 :: Assertion
case32 = expected @=? testStep (step indentIndentStyle { cBreakEnums = True }) input
  where
    expected = input
    input = unlines
      [ "data RejectionReason"
      , "  -- InvalidState"
      , "  = CancellationFailed"
      , "  | TotalAmountConfirmationInvalid"
      , "  -- InvalidApiUsage"
      , "  | AccessTokenNotActive"
      , "  | VersionNotFound"
      , "  -- ValidationFailed"
      , "  | BankAccountExists"
      , "  deriving stock (Generic, Show, Eq)"
      , "  deriving (ToJSON, FromJSON) via SnakeCaseLowercaseEnumEncoding RejectionReason"
      ]

case33 :: Assertion
case33 = expected @=? testStep (step indentIndentStyle { cBreakEnums = True, cBreakSingleConstructors = False }) input
  where
    input = unlines
      [ "module Some.Types where"
      , ""
      , "newtype NonEmpty a = NonEmpty { unNonEmpty :: a }"
      ]

    expected = unlines
      [ "module Some.Types where"
      , ""
      , "newtype NonEmpty a"
      , "  = NonEmpty { unNonEmpty :: a }"
      ]

case34 :: Assertion
case34 = expected @=? testStep (step indentIndentStyle { cVia = Indent 2 }) input
  where
    input = unlines
      [ "module Some.Types where"
      , ""
      , "newtype NonEmpty a = NonEmpty { unNonEmpty :: a }"
      , "     deriving (ToJSON, FromJSON) via Something Magic (NonEmpty a)"
      ]

    expected = unlines
      [ "module Some.Types where"
      , ""
      , "newtype NonEmpty a"
      , "  = NonEmpty { unNonEmpty :: a }"
      , "  deriving (ToJSON, FromJSON)"
      , "    via Something Magic (NonEmpty a)"
      ]

case35 :: Assertion
case35 = expected @=? testStep (step indentIndentStyle { cBreakEnums = True, cBreakSingleConstructors = False }) input
  where
    input = unlines
      [ "module Some.Types where"
      , ""
      , "data Foo = Foo"
      , "  { _transfer :: MonetaryAmount"
      , "      -> TransactionId"
      , "      -> m (Either CreditTransferError TransactionId)"
      , "  }"
      ]

    expected = unlines
      [ "module Some.Types where"
      , ""
      , "data Foo = Foo"
      , "  { _transfer :: MonetaryAmount -> TransactionId -> m (Either CreditTransferError TransactionId)"
      , "  }"
      ]

case36 :: Assertion
case36 = expected @=? testStep (step indentIndentStyle { cBreakEnums = True, cBreakSingleConstructors = False }) input
  where
    input = unlines
      [ "module Some.Types where"
      , ""
      , "data Foo = Foo"
      , "  { _transfer :: (a -> b)"
      , "      -> TransactionId"
      , "      -> m (Either CreditTransferError TransactionId)"
      , "  }"
      ]

    expected = unlines
      [ "module Some.Types where"
      , ""
      , "data Foo = Foo"
      , "  { _transfer :: (a -> b) -> TransactionId -> m (Either CreditTransferError TransactionId)"
      , "  }"
      ]

case37 :: Assertion
case37 = expected @=? testStep (step indentIndentStyle { cVia = Indent 2 }) input
  where
    input = unlines
      [ "module Some.Types where"
      , ""
      , "newtype UndoFlowData"
      , "  = UndoFlowData { flowDataDetails :: FlowDataDetails }"
      , "  deriving stock (Generic, Eq, Show)"
      , "  deriving (ToJSON, FromJSON)"
      , "    via AddConstTextFields '[\"type0\" := \"undo\","
      , "                             \"type1\" := \"undo\","
      , "                     \"reversal_indicator\" := \"Undo\"] FlowDataDetails"
      ]

    expected = unlines
      [ "module Some.Types where"
      , ""
      , "newtype UndoFlowData"
      , "  = UndoFlowData { flowDataDetails :: FlowDataDetails }"
      , "  deriving stock (Generic, Eq, Show)"
      , "  deriving (ToJSON, FromJSON)"
      , "    via AddConstTextFields '[\"type0\" := \"undo\", \"type1\" := \"undo\", \"reversal_indicator\" := \"Undo\"] FlowDataDetails"
      ]

case38 :: Assertion
case38 = expected @=? testStep (step indentIndentStyle { cVia = Indent 2 }) input
  where
    input = unlines
      [ "data Flat = Flat"
      , "  { foo :: Int"
      , "  , bar :: Text"
      , "  , baz :: Double"
      , "  , qux :: Bool"
      , "  }"
      , "  deriving stock (Generic, Show, Eq)"
      , "  deriving (FromJSON, ToJSON)"
      , "    via GenericEncoded"
      , "      '[ FieldLabelModifier :="
      , "        '[ \"foo\" ==> \"nestFoo#foo\""
      , "         , \"bar\" ==> \"nestBar#bar\""
      , "         , \"baz\" ==> \"nestFoo#baz\""
      , "         ]"
      , "       ]"
      , "      Flat"
      ]

    expected = unlines
      [ "data Flat"
      , "  = Flat"
      , "      { foo :: Int"
      , "      , bar :: Text"
      , "      , baz :: Double"
      , "      , qux :: Bool"
      , "      }"
      , "  deriving stock (Generic, Show, Eq)"
      , "  deriving (FromJSON, ToJSON)"
      , "    via GenericEncoded '[FieldLabelModifier := '[\"foo\" ==> \"nestFoo#foo\", \"bar\" ==> \"nestBar#bar\", \"baz\" ==> \"nestFoo#baz\"]] Flat"
      ]

case39 :: Assertion
case39 = expected @=? testStep (step indentIndentStyle { cVia = Indent 2 }) input
  where
    input = unlines
      [ "data CreditTransfer = CreditTransfer"
      , "  { nestedCreditorInfo :: CreditorInfo"
      , "  }"
      , "  deriving stock (Show, Eq, Generic)"
      , "  deriving (ToJSON, FromJSON) via"
      , "    ( UntaggedEncoded NordeaCreditTransfer"
      , "    & AddConstTextFields"
      , "      '[ \"request_type\" ':= \"credit_transfer\""
      , "       , \"provider\" ':= \"nordea\""
      , "       ]"
      , "    & FlattenFields '[\"nested_creditor_info\"]"
      , "    & RenameKeys"
      , "        '[ \"nested_creditor_info.creditor_agent_bic\" ==> \"creditor_agent_bic\""
      , "         , \"nested_creditor_info.creditor_iban\" ==> \"creditor_iban\""
      , "         , \"nested_creditor_info.creditor_name\" ==> \"creditor_name\""
      , "         , \"nested_creditor_info.creditor_account\" ==> \"creditor_account\""
      , "         ]"
      , "    )"
      ]

    expected = unlines
      [ "data CreditTransfer"
      , "  = CreditTransfer"
      , "      { nestedCreditorInfo :: CreditorInfo"
      , "      }"
      , "  deriving stock (Show, Eq, Generic)"
      , "  deriving (ToJSON, FromJSON)"
      , "    via (UntaggedEncoded NordeaCreditTransfer & AddConstTextFields '[\"request_type\" ':= \"credit_transfer\", \"provider\" ':= \"nordea\"] & FlattenFields '[\"nested_creditor_info\"] & RenameKeys '[\"nested_creditor_info.creditor_agent_bic\" ==> \"creditor_agent_bic\", \"nested_creditor_info.creditor_iban\" ==> \"creditor_iban\", \"nested_creditor_info.creditor_name\" ==> \"creditor_name\", \"nested_creditor_info.creditor_account\" ==> \"creditor_account\"])"
      ]

case40 :: Assertion
case40 = expected @=? testStep (step indentIndentStyle { cBreakSingleConstructors = False }) input
  where
    input = unlines
      [ "module X where"
      , ""
      , "data a :==> b    ="
      , "                  Arr a b"
      ]

    expected = unlines
      [ "module X where"
      , ""
      , "data a :==> b = Arr a b"
      ]

case41 :: Assertion
case41 = expected @=? testStep (step indentIndentStyle) input
  where
    input = expected

    expected = unlines
      [ "module X where"
      , ""
      , "data Callback"
      , "  -- | Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor"
      , "  --   incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis"
      , "  --   nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."
      , "  --   Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore"
      , "  --   eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident,"
      , "  --   sunt in culpa qui officia deserunt mollit anim id est laborum."
      , "  = KafkaTopic"
      , "      { callbackTopic :: CallbackTopic"
      , "        -- ^ Name of topic to send updates to"
      , "      , callbackFormat :: CallbackFormat"
      , "        -- ^ The format used to send these updates"
      , "      }"
      , "  deriving stock (Generic, Eq, Show)"
      , "  deriving (ToJSON, FromJSON) via IdiomaticWithDescription CallbackDesc Callback"
      , "  deriving (HasGen) via Generically Callback"
      , "  deriving (FromField) via JsonField Callback"
      ]

sameSameStyle :: Config
sameSameStyle = Config SameLine SameLine 2 2 False True SameLine

sameIndentStyle :: Config
sameIndentStyle = Config SameLine (Indent 2) 2 2 False True SameLine

indentSameStyle :: Config
indentSameStyle = Config (Indent 2) SameLine 2 2 False True SameLine

indentIndentStyle :: Config
indentIndentStyle = Config (Indent 2) (Indent 2) 2 2 False True SameLine

indentIndentStyle4 :: Config
indentIndentStyle4 = Config (Indent 4) (Indent 4) 4 4 False True SameLine
