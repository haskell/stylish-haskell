--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Step
    ( Lines
    , Module
    , GHCModule
    , Step (..)
    , makeStep
    ) where


--------------------------------------------------------------------------------
import qualified Language.Haskell.Exts as H

import qualified SrcLoc                as S
import qualified GHC.Hs                as Hs


--------------------------------------------------------------------------------
type Lines = [String]


--------------------------------------------------------------------------------
-- | Concrete HSE module type
type Module = (H.Module H.SrcSpanInfo, [H.Comment]) 

-- | Concrete GHC module type
type GHCModule = (S.Located (Hs.HsModule Hs.GhcPs))


--------------------------------------------------------------------------------
data Step = Step
    { stepName   :: String
    , stepFilter :: Either (Lines -> Module -> Lines) (Lines -> GHCModule -> Lines)
    }

--------------------------------------------------------------------------------
makeStep :: String -> Either (Lines -> Module -> Lines) (Lines -> GHCModule -> Lines) -> Step
makeStep = Step


