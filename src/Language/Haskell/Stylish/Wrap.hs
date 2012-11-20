--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Wrap
    ( WrapStyle (..)
    , Wrap (..)
    , wrapWith
    , regularWrap
    , utrechtWrap
    ) where


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Util


--------------------------------------------------------------------------------
data WrapStyle
    = Regular
    | Utrecht
    deriving (Show)


--------------------------------------------------------------------------------
data Wrap
    = String String
    | Space
    | Comma
    deriving (Show)


--------------------------------------------------------------------------------
wrapWith :: WrapStyle  -- ^ Wrapping style to use
         -> Int        -- ^ Maximum line width
         -> [Wrap]     -- ^ Stuff to wrap
         -> Lines      -- ^ Resulting lines
wrapWith Regular = regularWrap
wrapWith Utrecht = utrechtWrap


--------------------------------------------------------------------------------
regularWrap :: Int     -- ^ Maximum line width
            -> [Wrap]  -- ^ Stuff to wrap
            -> Lines   -- ^ Resulting lines
regularWrap maxWidth wraps =
    let (leading : strs) = regularJoin wraps
    in wrap " " maxWidth (length leading) leading strs


--------------------------------------------------------------------------------
regularJoin :: [Wrap] -> [String]
regularJoin wraps = go wraps
  where
    go (String x : String y : xs) = go (String (x ++ y) : xs)
    go (String x : Comma : xs)    = (x ++ ",") : go xs
    go (String x : xs)            = x : go xs
    go (Space : xs)               = go xs
    go []                         = []
    go ws                         = error $
        "Language.Haskell.Stylish.Wrap.regularJoin: go: " ++
        show ws ++ " is invalid, in: " ++ show wraps


--------------------------------------------------------------------------------
utrechtWrap :: Int      -- ^ Maximum line width
            -> [Wrap]   -- ^ Stuff to wrap
            -> Lines    -- ^ Resulting lines
utrechtWrap maxWidth wraps =
    -- If we can put everything on one line, we use regular wrapping
    case regularWrap maxWidth wraps of
        [line]                    -> [line]
        xs
            | length utrechts < 3 -> xs  -- Should not happen!
            | otherwise           ->
                let (leading : strs) = utrechts
                in wrap "" maxWidth (length leading - 1)
                    (leading ++ " ") (init strs) ++
                    [replicate (length leading - 1) ' ' ++ last strs]
  where
    utrechts = utrechtJoin wraps


--------------------------------------------------------------------------------
utrechtJoin :: [Wrap] -> [String]
utrechtJoin wraps = case wraps of
    (String x : xs) -> x : go xs  -- Never join first string in utrecht style
    _               -> go wraps   -- Should this ever happen?
  where
    go (Space : xs)               = go xs
    go (String x : String y : xs) = go (String (x ++ y) : xs)
    go (Comma : String x : xs)    = (", " ++ x ) : go xs
    go (String x : xs)            = x : go xs
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
