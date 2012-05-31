--------------------------------------------------------------------------------
module StylishHaskell.Stylish.Tabs
    ( stylish
    ) where


--------------------------------------------------------------------------------
import           StylishHaskell.Stylish


--------------------------------------------------------------------------------
removeTabs :: Int -> String -> String
removeTabs spaces = concatMap removeTabs'
  where
    removeTabs' '\t' = replicate spaces ' '
    removeTabs' x    = [x]


--------------------------------------------------------------------------------
stylish :: Int -> Stylish
stylish spaces ls _ = map (removeTabs spaces) ls
