--------------------------------------------------------------------------------
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Test.Framework               (defaultMain)


--------------------------------------------------------------------------------
import qualified StylishHaskell.Stylish.Imports.Tests
import qualified StylishHaskell.Stylish.LanguagePragmas.Tests
import qualified StylishHaskell.Stylish.Tabs.Tests
import qualified StylishHaskell.Stylish.TrailingWhitespace.Tests
import qualified StylishHaskell.Stylish.UnicodeSyntax.Tests


--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain
    [ StylishHaskell.Stylish.Imports.Tests.tests
    , StylishHaskell.Stylish.LanguagePragmas.Tests.tests
    , StylishHaskell.Stylish.Tabs.Tests.tests
    , StylishHaskell.Stylish.TrailingWhitespace.Tests.tests
    , StylishHaskell.Stylish.UnicodeSyntax.Tests.tests
    ]
