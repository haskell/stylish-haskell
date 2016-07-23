--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Step.SimpleAlign.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                            (Test, testGroup)
import           Test.Framework.Providers.HUnit            (testCase)
import           Test.HUnit                                (Assertion, (@=?))


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Step.SimpleAlign
import           Language.Haskell.Stylish.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Language.Haskell.Stylish.Step.SimpleAlign.Tests"
    [ testCase "case 01" case01
    , testCase "case 02" case02
    , testCase "case 03" case03
    , testCase "case 04" case04
    , testCase "case 05" case05
    , testCase "case 06" case06
    , testCase "case 07" case07
    ]


--------------------------------------------------------------------------------
case01 :: Assertion
case01 = expected @=? testStep (step 80 defaultConfig) input
  where
    input = unlines
        [ "eitherToMaybe e = case e of"
        , "    Left _ -> Nothing"
        , "    Right x -> Just x"
        ]

    expected = unlines
        [ "eitherToMaybe e = case e of"
        , "    Left _  -> Nothing"
        , "    Right x -> Just x"
        ]


--------------------------------------------------------------------------------
case02 :: Assertion
case02 = expected @=? testStep (step 80 defaultConfig) input
  where
    input = unlines
        [ "eitherToMaybe (Left _) = Nothing"
        , "eitherToMaybe (Right x) = Just x"
        ]

    expected = unlines
        [ "eitherToMaybe (Left _)  = Nothing"
        , "eitherToMaybe (Right x) = Just x"
        ]


--------------------------------------------------------------------------------
case03 :: Assertion
case03 = expected @=? testStep (step 80 defaultConfig) input
  where
    input = unlines
        [ "heady def [] = def"
        , "heady _ (x : _) = x"
        ]

    expected = unlines
        [ "heady def []    = def"
        , "heady _ (x : _) = x"
        ]


--------------------------------------------------------------------------------
case04 :: Assertion
case04 = expected @=? testStep (step 80 defaultConfig) input
  where
    input = unlines
        [ "data Foo = Foo"
        , "    { foo :: Int"
        , "    , barqux :: String"
        , "    } deriving (Show)"
        ]

    expected = unlines
        [ "data Foo = Foo"
        , "    { foo    :: Int"
        , "    , barqux :: String"
        , "    } deriving (Show)"
        ]


--------------------------------------------------------------------------------
case05 :: Assertion
case05 = input @=? testStep (step 80 defaultConfig) input
  where
    -- Don't attempt to align this since a field spans multiple lines
    input = unlines
        [ "data Foo = Foo"
        , "    { foo :: Int"
        , "    , barqux"
        , "         :: String"
        , "    } deriving (Show)"
        ]


--------------------------------------------------------------------------------
case06 :: Assertion
case06 =
    -- 22 max columns is /just/ enough to align this stuff.
    expected @=? testStep (step 22 defaultConfig) input
  where
    input = unlines
        [ "data Foo = Foo"
        , "    { foo :: String"
        , "    , barqux :: Int"
        , "    }"
        ]

    expected = unlines
        [ "data Foo = Foo"
        , "    { foo    :: String"
        , "    , barqux :: Int"
        , "    }"
        ]


--------------------------------------------------------------------------------
case07 :: Assertion
case07 =
    -- 21 max columns is /just NOT/ enough to align this stuff.
    expected @=? testStep (step 21 defaultConfig) input
  where
    input = unlines
        [ "data Foo = Foo"
        , "    { foo :: String"
        , "    , barqux :: Int"
        , "    }"
        ]

    expected = unlines
        [ "data Foo = Foo"
        , "    { foo :: String"
        , "    , barqux :: Int"
        , "    }"
        ]
