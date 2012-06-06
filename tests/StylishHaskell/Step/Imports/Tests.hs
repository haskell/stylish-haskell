--------------------------------------------------------------------------------
module StylishHaskell.Step.Imports.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     ((@=?))


--------------------------------------------------------------------------------
import           StylishHaskell.Step.Imports
import           StylishHaskell.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "StylishHaskell.Step.Imports.Tests"
    [ case01
    , case02
    , case03
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
    , "import Herp.Derp.Internals hiding (foo)"
    , ""
    , "herp = putStrLn \"import Hello world\""
    ]


--------------------------------------------------------------------------------
case01 :: Test
case01 = testCase "case 01" $ expected @=? testStep (step Global) input
  where
    expected = unlines
        [ "module Herp where"
        , ""
        , "import           Control.Monad"
        , "import           Data.Map            (Map, insert, lookup, (!))"
        , "import qualified Data.Map            as M"
        , ""
        , "import           Herp.Derp.Internals hiding (foo)"
        , ""
        , "herp = putStrLn \"import Hello world\""
        ]


--------------------------------------------------------------------------------
case02 :: Test
case02 = testCase "case 02" $ expected @=? testStep (step Groups) input
  where
    expected = unlines
        [ "module Herp where"
        , ""
        , "import           Control.Monad"
        , "import           Data.Map      (Map, insert, lookup, (!))"
        , "import qualified Data.Map      as M"
        , ""
        , "import Herp.Derp.Internals hiding (foo)"
        , ""
        , "herp = putStrLn \"import Hello world\""
        ]


--------------------------------------------------------------------------------
case03 :: Test
case03 = testCase "case 03" $ expected @=? testStep (step None) input
  where
    expected = unlines
        [ "module Herp where"
        , ""
        , "import Control.Monad"
        , "import Data.Map (Map, insert, lookup, (!))"
        , "import qualified Data.Map as M"
        , ""
        , "import Herp.Derp.Internals hiding (foo)"
        , ""
        , "herp = putStrLn \"import Hello world\""
        ]
