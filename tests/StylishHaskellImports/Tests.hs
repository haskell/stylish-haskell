module StylishHaskellImports.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     ((@=?))


--------------------------------------------------------------------------------
import           StylishHaskellImports         


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "StylishHaskellImports.Tests"
    [ case01
    ]


--------------------------------------------------------------------------------
case01 :: Test
case01 = testCase "case 01" $ expected @=? stylishHaskellImports input
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
