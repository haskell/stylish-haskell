module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Test.Framework              (defaultMain)


--------------------------------------------------------------------------------
import qualified StylishHaskellImports.Tests


--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain
    [ StylishHaskellImports.Tests.tests
    ]
