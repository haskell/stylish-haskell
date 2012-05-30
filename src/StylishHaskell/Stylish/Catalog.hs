--------------------------------------------------------------------------------
module StylishHaskell.Stylish.Catalog
    ( catalog
    , fromCatalog
    ) where


--------------------------------------------------------------------------------
import           Data.Map                                  (Map)
import qualified Data.Map                                  as M
import           Data.Maybe                                (mapMaybe)


--------------------------------------------------------------------------------
import           StylishHaskell.Stylish
import qualified StylishHaskell.Stylish.Imports
import qualified StylishHaskell.Stylish.LanguagePragmas
import qualified StylishHaskell.Stylish.Tabs
import qualified StylishHaskell.Stylish.TrailingWhitespace
import qualified StylishHaskell.Stylish.UnicodeSyntax


--------------------------------------------------------------------------------
catalog :: Map String Stylish
catalog = M.fromList
    [ ("Imports",            StylishHaskell.Stylish.Imports.stylish)
    , ("LanguagePragmas",    StylishHaskell.Stylish.LanguagePragmas.stylish)
    , ("Tabs",               StylishHaskell.Stylish.Tabs.stylish)
    , ("TrailingWhitespace", StylishHaskell.Stylish.TrailingWhitespace.stylish)
    , ("UnicodeSyntax",      StylishHaskell.Stylish.UnicodeSyntax.stylish)
    ]


--------------------------------------------------------------------------------
fromCatalog :: [String] -> [Stylish]
fromCatalog = mapMaybe (`M.lookup` catalog)
