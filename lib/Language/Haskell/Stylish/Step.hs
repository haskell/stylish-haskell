--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Step
    ( Lines
    , OldModule
    , Step (..)
    , makeStep
    , oldMakeStep
    ) where


--------------------------------------------------------------------------------
import qualified Language.Haskell.Exts as H
import           Language.Haskell.Stylish.Module

--------------------------------------------------------------------------------
type OldModule = (H.Module H.SrcSpanInfo, [H.Comment])

--------------------------------------------------------------------------------
data Step
  = Step
    { stepName :: String
    , stepFilter :: Lines -> Module -> Lines
    }
  | OldStep
    { stepName :: String
    , oldStepFilter :: Lines -> OldModule -> Lines
    }

--------------------------------------------------------------------------------
makeStep :: String -> (Lines -> Module -> Lines) -> Step
makeStep = Step

oldMakeStep :: String -> (Lines -> OldModule -> Lines) -> Step
oldMakeStep = OldStep
