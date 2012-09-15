--------------------------------------------------------------------------------
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Test.Framework                               (defaultMain)


--------------------------------------------------------------------------------
import qualified Language.Haskell.Stylish.Parse.Tests
import qualified Language.Haskell.Stylish.Step.Imports.Tests
import qualified Language.Haskell.Stylish.Step.LanguagePragmas.Tests
import qualified Language.Haskell.Stylish.Step.Records.Tests
import qualified Language.Haskell.Stylish.Step.Tabs.Tests
import qualified Language.Haskell.Stylish.Step.TrailingWhitespace.Tests
import qualified Language.Haskell.Stylish.Step.UnicodeSyntax.Tests


--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain
    [ Language.Haskell.Stylish.Parse.Tests.tests
    , Language.Haskell.Stylish.Step.Imports.Tests.tests
    , Language.Haskell.Stylish.Step.LanguagePragmas.Tests.tests
    , Language.Haskell.Stylish.Step.Records.Tests.tests
    , Language.Haskell.Stylish.Step.Tabs.Tests.tests
    , Language.Haskell.Stylish.Step.TrailingWhitespace.Tests.tests
    , Language.Haskell.Stylish.Step.UnicodeSyntax.Tests.tests
    ]
