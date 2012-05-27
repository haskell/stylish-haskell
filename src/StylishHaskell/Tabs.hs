--------------------------------------------------------------------------------
module StylishHaskell.Tabs
    ( stylish
    ) where


--------------------------------------------------------------------------------
import           StylishHaskell.Stylish


--------------------------------------------------------------------------------
removeTabs :: String -> String
removeTabs = concatMap removeTabs'
  where
    removeTabs' '\t' = "    "
    removeTabs' x    = [x]


--------------------------------------------------------------------------------
stylish :: Stylish
stylish ls _ = map removeTabs ls
