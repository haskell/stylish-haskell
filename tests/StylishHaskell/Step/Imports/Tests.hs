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
case01 = testCase "case 01" $ expected @=? testStep (step True) input
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
case02 = testCase "case 02" $ expected @=? testStep (step False) input
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
