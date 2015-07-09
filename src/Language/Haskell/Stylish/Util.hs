--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Util
    ( nameToString
    , indent
    , padRight
    , everything
    , infoPoints
    , wrap
    , wrapRest

    , withHead
    , withInit
    , withTail
    , withLast
    ) where


--------------------------------------------------------------------------------
import           Control.Arrow                   ((&&&), (>>>))
import           Data.Data                       (Data)
import qualified Data.Generics                   as G
import           Data.Maybe                      (maybeToList)
import           Data.Typeable                   (cast)
import qualified Language.Haskell.Exts.Annotated as H


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Step


--------------------------------------------------------------------------------
nameToString :: H.Name l -> String
nameToString (H.Ident _ str)  = str
nameToString (H.Symbol _ str) = str


--------------------------------------------------------------------------------
indent :: Int -> String -> String
indent len = (indentPrefix len ++)


--------------------------------------------------------------------------------
indentPrefix :: Int -> String
indentPrefix = (`replicate` ' ')


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
wrap maxWidth leading ind strs' = wrap' leading strs'
  where
    wrap' ss [] = [ss]
    wrap' ss (str:strs)
        | overflows ss str =
            ss : wrapRest maxWidth ind (str:strs)
        | otherwise = wrap' (ss ++ " " ++ str) strs

    overflows ss str = (length ss + length str) >= maxWidth
        && ind + length str  <= maxWidth



--------------------------------------------------------------------------------
wrapRest :: Int
         -> Int
         -> [String]
         -> Lines
wrapRest maxWidth ind = reverse . wrapRest' [] ""
  where
    wrapRest' ls ss []
        | null ss = ls
        | otherwise = ss:ls
    wrapRest' ls ss (str:strs)
        | null ss = wrapRest' ls (indent ind str) strs
        | overflows ss str = wrapRest' (ss:ls) "" (str:strs)
        | otherwise = wrapRest' ls (ss ++ " " ++ str) strs

    overflows ss str = (length ss + length str + 1) >= maxWidth


--------------------------------------------------------------------------------
withHead :: (a -> a) -> [a] -> [a]
withHead _ []       = []
withHead f (x : xs) = f x : xs


--------------------------------------------------------------------------------
withLast :: (a -> a) -> [a] -> [a]
withLast _ []       = []
withLast f (x : []) = [f x]
withLast f (x : xs) = x : withLast f xs


--------------------------------------------------------------------------------
withInit :: (a -> a) -> [a] -> [a]
withInit _ []       = []
withInit _ (x : []) = [x]
withInit f (x : xs) = f x : withInit f xs

--------------------------------------------------------------------------------
withTail :: (a -> a) -> [a] -> [a]
withTail _ [] = []
withTail f (x : xs) = x : map f xs
