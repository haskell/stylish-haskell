module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Test.Framework               (defaultMain)


--------------------------------------------------------------------------------
import qualified StylishHaskell.Imports.Tests


--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain
    [ StylishHaskell.Imports.Tests.tests
    ]
