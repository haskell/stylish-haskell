--------------------------------------------------------------------------------
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Test.Framework               (defaultMain)


--------------------------------------------------------------------------------
import qualified StylishHaskell.Imports.Tests
import qualified StylishHaskell.LanguagePragmas.Tests


--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain
    [ StylishHaskell.Imports.Tests.tests
    , StylishHaskell.LanguagePragmas.Tests.tests
    ]
