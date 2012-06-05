--------------------------------------------------------------------------------
module StylishHaskell.Step.Tabs
    ( step
    ) where


--------------------------------------------------------------------------------
import           StylishHaskell.Step


--------------------------------------------------------------------------------
removeTabs :: Int -> String -> String
removeTabs spaces = concatMap removeTabs'
  where
    removeTabs' '\t' = replicate spaces ' '
    removeTabs' x    = [x]


--------------------------------------------------------------------------------
step :: Int -> Step
step spaces ls _ = map (removeTabs spaces) ls
