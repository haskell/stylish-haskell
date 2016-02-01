--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Step.Tabs
    ( step
    ) where


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Step


--------------------------------------------------------------------------------
removeTabs :: Int -> String -> String
removeTabs spaces = concatMap removeTabs'
  where
    removeTabs' '\t' = replicate spaces ' '
    removeTabs' x    = [x]


--------------------------------------------------------------------------------
step :: Int -> Step
step spaces = makeStep "Tabs" $ \ls _ -> map (removeTabs spaces) ls
