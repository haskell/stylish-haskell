--------------------------------------------------------------------------------
module StylishHaskell.Stylish.Imports.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     ((@=?))


--------------------------------------------------------------------------------
import           StylishHaskell.Stylish.Imports
import           StylishHaskell.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "StylishHaskell.Stylish.Imports.Tests"
    [ case01
    , case02
    ]


--------------------------------------------------------------------------------
input :: String
input = unlines
    [ "module Herp where"
    , ""
    , "import qualified Data.Map  as M"
    , "import Control.Monad"
    , "import       Data.Map     (lookup, (!), insert, Map)"
    , ""
    , "import Herp.Derp.Internals"
    , ""
    , "herp = putStrLn \"import Hello world\""
    ]


--------------------------------------------------------------------------------
case01 :: Test
case01 = testCase "case 01" $ expected @=? testStylish (stylish True) input
  where
    expected = unlines
        [ "module Herp where"
        , ""
        , "import           Control.Monad"
        , "import           Data.Map            (Map, insert, lookup, (!))"
        , "import qualified Data.Map            as M"
        , ""
        , "import           Herp.Derp.Internals"
        , ""
        , "herp = putStrLn \"import Hello world\""
        ]


--------------------------------------------------------------------------------
case02 :: Test
case02 = testCase "case 02" $ expected @=? testStylish (stylish False) input
  where
    expected = unlines
        [ "module Herp where"
        , ""
        , "import Control.Monad"
        , "import Data.Map (Map, insert, lookup, (!))"
        , "import qualified Data.Map as M"
        , ""
        , "import Herp.Derp.Internals"
        , ""
        , "herp = putStrLn \"import Hello world\""
        ]
