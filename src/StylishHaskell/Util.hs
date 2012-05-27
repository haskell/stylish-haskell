--------------------------------------------------------------------------------
module StylishHaskell.Util
    ( nameToString
    , padRight
    ) where


--------------------------------------------------------------------------------
import qualified Language.Haskell.Exts.Annotated as H


--------------------------------------------------------------------------------
-- | TODO: put this in utilities?
nameToString :: H.Name l -> String
nameToString (H.Ident _ str)  = str
nameToString (H.Symbol _ str) = str


--------------------------------------------------------------------------------
padRight :: Int -> String -> String
padRight len str = str ++ replicate (len - length str) ' '
