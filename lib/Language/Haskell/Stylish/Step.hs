--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Step
    ( Lines
    , Step (..)
    , makeStep
    ) where


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Module

--------------------------------------------------------------------------------
data Step = Step
    { stepName   :: String
    , stepFilter :: Lines -> Module -> Lines
    }

--------------------------------------------------------------------------------
makeStep :: String -> (Lines -> Module -> Lines) -> Step
makeStep = Step
