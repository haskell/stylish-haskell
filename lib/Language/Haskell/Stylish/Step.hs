--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Step
    ( Lines
    , Module
    , Step (..)
    , makeStep
    ) where


--------------------------------------------------------------------------------
import qualified Language.Haskell.Exts as H


--------------------------------------------------------------------------------
type Lines = [String]


--------------------------------------------------------------------------------
-- | Concrete module type
type Module = (H.Module H.SrcSpanInfo, [H.Comment])


--------------------------------------------------------------------------------
data Step = Step
    { stepName   :: String
    , stepFilter :: Lines -> Module -> Lines
    }


--------------------------------------------------------------------------------
makeStep :: String -> (Lines -> Module -> Lines) -> Step
makeStep = Step
