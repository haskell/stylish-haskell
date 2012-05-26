--------------------------------------------------------------------------------
module StylishHaskell.Util
    ( nameToString
    ) where


--------------------------------------------------------------------------------
import qualified Language.Haskell.Exts.Annotated as H


--------------------------------------------------------------------------------
-- | TODO: put this in utilities?
nameToString :: H.Name l -> String
nameToString (H.Ident _ str)  = str
nameToString (H.Symbol _ str) = str
