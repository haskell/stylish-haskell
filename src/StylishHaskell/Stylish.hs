--------------------------------------------------------------------------------
module StylishHaskell.Stylish
    ( Lines
    , Module
    , Stylish
    ) where


--------------------------------------------------------------------------------
import qualified Language.Haskell.Exts.Annotated as H


--------------------------------------------------------------------------------
type Lines = [String]


--------------------------------------------------------------------------------
-- | Concrete module type
type Module = (H.Module H.SrcSpanInfo, [H.Comment])


--------------------------------------------------------------------------------
type Stylish = Lines -> Module -> Lines
