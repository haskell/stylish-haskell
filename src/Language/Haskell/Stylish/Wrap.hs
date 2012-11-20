--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Wrap
    ( Wrap (..)
    , regularWrap
    ) where


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Util


--------------------------------------------------------------------------------
data Wrap
    = Open String
    | Close String
    | String String
    | Space
    | Comma
    deriving (Show)


--------------------------------------------------------------------------------
regularWrap :: Int     -- ^ Maximum line width
            -> Int     -- ^ Indentation
            -> [Wrap]  -- ^ Stuff to wrap
            -> Lines   -- ^ Resulting lines
regularWrap maxWidth indentation wraps =
    let (leading, strs) = regularJoin wraps
    in wrap " " maxWidth indentation leading strs


--------------------------------------------------------------------------------
regularJoin :: [Wrap] -> (String, [String])
regularJoin wraps = case wraps of
    (Open x : xs) -> (x, go xs)
    _             -> error $ "Language.Haskell.Stylish.Wrap.regularJoin: " ++
        "wrap spec should start with Open but got: " ++ show wraps
  where
    go (String x : String y : xs) = go (String (x ++ y) : xs)
    go (String x : Comma : xs)    = (x ++ ",") : go xs
    go (String x : Space : xs)    = x : go xs
    go (String x : Close y : [])  = [x ++ y]
    go (Close x : [])             = [x]
    go (Space : xs)               = go xs
    go []                         = []
    go ws                         = error $
        "Language.Haskell.Stylish.Wrap.regularJoin: go: " ++
        show ws ++ " is invalid, in: " ++ show wraps


--------------------------------------------------------------------------------
wrap :: String    -- ^ Optional space string
     -> Int       -- ^ Maximum line width
     -> Int       -- ^ Indentation
     -> String    -- ^ Leading string
     -> [String]  -- ^ Strings to add/wrap
     -> Lines     -- ^ Resulting lines
wrap space maxWidth ind leading strs =
    let (ls, curr, _) = foldl step ([], leading, length leading) strs
    in ls ++ [curr]
  where
    -- TODO: In order to optimize this, use a difference list instead of a
    -- regular list for 'ls'.
    step (ls, curr, width) str
        | nextLine  = (ls ++ [curr], indent ind str, ind + len)
        | otherwise = (ls, curr ++ space ++ str, width')
      where
        -- Put it on the next line if it would make the current line too long,
        -- AND if it doesn't make the next line too long.
        nextLine = width' > maxWidth && ind + len <= maxWidth
        len      = length str
        width'   = width + length space + len
