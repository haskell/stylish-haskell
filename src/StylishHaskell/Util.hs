--------------------------------------------------------------------------------
module StylishHaskell.Util
    ( nameToString
    , padRight
    , everything
    , infoPoints
    , wrap
    ) where


--------------------------------------------------------------------------------
import           Control.Arrow                   ((&&&), (>>>))
import           Data.Data                       (Data)
import qualified Data.Generics                   as G
import           Data.Maybe                      (maybeToList)
import           Data.Typeable                   (cast)
import qualified Language.Haskell.Exts.Annotated as H


--------------------------------------------------------------------------------
import           StylishHaskell.Stylish


--------------------------------------------------------------------------------
nameToString :: H.Name l -> String
nameToString (H.Ident _ str)  = str
nameToString (H.Symbol _ str) = str


--------------------------------------------------------------------------------
padRight :: Int -> String -> String
padRight len str = str ++ replicate (len - length str) ' '


--------------------------------------------------------------------------------
everything :: (Data a, Data b) => a -> [b]
everything = G.everything (++) (maybeToList . cast)


--------------------------------------------------------------------------------
infoPoints :: H.SrcSpanInfo -> [((Int, Int), (Int, Int))]
infoPoints = H.srcInfoPoints >>> map (H.srcSpanStart &&& H.srcSpanEnd)


--------------------------------------------------------------------------------
wrap :: Int       -- ^ Maximum line width
     -> String    -- ^ Leading string
     -> Int       -- ^ Indentation
     -> [String]  -- ^ Strings to add/wrap
     -> Lines     -- ^ Resulting lines
wrap maxWidth leading indent strs =
    let (ls, curr, _) = foldl step ([], leading, length leading) strs
    in ls ++ [curr]
  where
    -- TODO: In order to optimize this, use a difference list instead of a
    -- regular list for 'ls'.
    step (ls, curr, width) str
        | width' > maxWidth = (ls ++ [curr], spaces ++ str, indent + len)
        | otherwise         = (ls, curr ++ " " ++ str, width')
      where
        len    = length str
        width' = width + 1 + len

    spaces = replicate indent ' '
