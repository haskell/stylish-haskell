--------------------------------------------------------------------------------
module StylishHaskell.Step
    ( Lines
    , Module
    , Step
    ) where


--------------------------------------------------------------------------------
import qualified Language.Haskell.Exts.Annotated as H


--------------------------------------------------------------------------------
type Lines = [String]


--------------------------------------------------------------------------------
-- | Concrete module type
type Module = (H.Module H.SrcSpanInfo, [H.Comment])


--------------------------------------------------------------------------------
type Step = Lines -> Module -> Lines
