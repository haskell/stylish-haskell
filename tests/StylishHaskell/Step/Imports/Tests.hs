--------------------------------------------------------------------------------
module StylishHaskell.Step.Imports.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (Assertion, (@=?))


--------------------------------------------------------------------------------
import           StylishHaskell.Step.Imports
import           StylishHaskell.Tests.Util


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "StylishHaskell.Step.Imports.Tests"
    [ testCase "case 01" case01
    , testCase "case 02" case02
    , testCase "case 03" case03
    , testCase "case 04" case04
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
    , "import HURR"
    , ""
    , "herp = putStrLn \"import Hello world\""
    ]


--------------------------------------------------------------------------------
case01 :: Assertion
case01 = expected @=? testStep (step 80 Global) input
  where
    expected = unlines
        [ "module Herp where"
        , ""
        , "import           Control.Monad"
        , "import           Data.Map            (Map, insert, lookup, (!))"
        , "import qualified Data.Map            as M"
        , ""
        , "import           Herp.Derp.Internals hiding (foo)"
        , "import           HURR"
        , ""
        , "herp = putStrLn \"import Hello world\""
        ]


--------------------------------------------------------------------------------
case02 :: Assertion
case02 = expected @=? testStep (step 80 Group) input
  where
    expected = unlines
        [ "module Herp where"
        , ""
        , "import           Control.Monad"
        , "import           Data.Map      (Map, insert, lookup, (!))"
        , "import qualified Data.Map      as M"
        , ""
        , "import Herp.Derp.Internals hiding (foo)"
        , "import HURR"
        , ""
        , "herp = putStrLn \"import Hello world\""
        ]


--------------------------------------------------------------------------------
case03 :: Assertion
case03 = expected @=? testStep (step 80 None) input
  where
    expected = unlines
        [ "module Herp where"
        , ""
        , "import Control.Monad"
        , "import Data.Map (Map, insert, lookup, (!))"
        , "import qualified Data.Map as M"
        , ""
        , "import Herp.Derp.Internals hiding (foo)"
        , "import HURR"
        , ""
        , "herp = putStrLn \"import Hello world\""
        ]


--------------------------------------------------------------------------------
case04 :: Assertion
case04 = expected @=? testStep (step 80 Global) input'
  where
    input' =
        "import Data.Aeson.Types (object, typeMismatch, FromJSON(..)," ++
        "ToJSON(..), Value(..), parseEither, (.!=), (.:), (.:?), (.=))"

    expected = unlines
        [ "import           Data.Aeson.Types (FromJSON(..), ToJSON(..), Value(..), object,"
        , "                                  parseEither, typeMismatch, (.!=), (.:), (.:?),"
        , "                                  (.=))"
        ]
