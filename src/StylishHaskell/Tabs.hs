--------------------------------------------------------------------------------
module StylishHaskell.Tabs
    ( stylish
    ) where


--------------------------------------------------------------------------------
import           StylishHaskell.Block
import           StylishHaskell.Parse


--------------------------------------------------------------------------------
removeTabs :: String -> String
removeTabs = concatMap removeTabs'
  where
    removeTabs' '\t' = "    "
    removeTabs' x    = [x]


--------------------------------------------------------------------------------
stylish :: Lines -> Module -> Lines
stylish ls _ = map removeTabs ls
