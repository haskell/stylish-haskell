module StylishHaskell.Imports.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     ((@=?))


--------------------------------------------------------------------------------
import           StylishHaskell.Editor
import           StylishHaskell.Imports
import           StylishHaskell.Parse


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "StylishHaskellImports.Tests"
    [ case01
    ]


--------------------------------------------------------------------------------
case01 :: Test
case01 = testCase "case 01" $ expected @=? testStylish input
  where
    input = unlines
        [ "module Herp where"
        , ""
        , "import qualified Data.Map  as M"
        , "import Control.Monad"
        , "import       Data.Map     (Map)"
        , ""
        , "import Herp.Derp.Internals"
        , ""
        , "herp = putStrLn \"import Hello world\""
        ]

    expected = unlines
        [ "module Herp where"
        , ""
        , "import           Control.Monad"
        , "import           Data.Map            (Map)"
        , "import qualified Data.Map            as M"
        , ""
        , "import           Herp.Derp.Internals"
        , ""
        , "herp = putStrLn \"import Hello world\""
        ]


--------------------------------------------------------------------------------
testStylish :: String -> String
testStylish str = case parseModule Nothing str of
    Left err      -> error err
    Right module' -> unlines $ applyChanges (stylish ls module') ls
  where
    ls = lines str
