--------------------------------------------------------------------------------
{-# LANGUAGE UnicodeSyntax #-}
module StylishHaskell.Stylish.UnicodeSyntax
    ( stylish
    ) where


--------------------------------------------------------------------------------
import           Data.List                       (isPrefixOf, sort)
import           Data.Map                        (Map)
import qualified Data.Map                        as M
import           Data.Maybe                      (maybeToList)
import qualified Language.Haskell.Exts.Annotated as H


--------------------------------------------------------------------------------
import           StylishHaskell.Block
import           StylishHaskell.Editor
import           StylishHaskell.Stylish
import           StylishHaskell.Util


--------------------------------------------------------------------------------
unicodeReplacements :: Map String String
unicodeReplacements = M.fromList
    [ ("->", "→")
    ]


--------------------------------------------------------------------------------
replaceAll :: [(Int, [(Int, String)])] -> Lines -> [Change String]
replaceAll positions ls =
    zipWith changeLine' positions $ selectLines (map fst positions) ls
  where
    changeLine' (r, ns) str = changeLine r $ return $ flip applyChanges str
        [ change (Block c ec) repl
        | (c, needle) <- sort ns
        , let ec = c + length needle - 1
        , repl <- maybeToList $ M.lookup needle unicodeReplacements
        ]



--------------------------------------------------------------------------------
selectLines :: [Int] -> Lines -> [String]
selectLines = go 1
  where
    go _ []      _         = []
    go _ _       []        = []
    go r (x : xs) (l : ls)
        | r == x                    = l : go (r + 1) xs ls
        | otherwise                 = go (r + 1) (x : xs) ls


--------------------------------------------------------------------------------
groupPerLine :: [((Int, Int), a)] -> [(Int, [(Int, a)])]
groupPerLine = M.toList . M.fromListWith (++) .
    map (\((r, c), x) -> (r, [(c, x)]))


--------------------------------------------------------------------------------
types :: H.Module H.SrcSpanInfo -> Lines -> [((Int, Int), String)]
types module' ls =
    [ (pos, "->")
    | H.TyFun _ t1 t2 <- everything module'
    , let start = H.srcSpanEnd $ H.srcInfoSpan $ H.ann t1
    , let end   = H.srcSpanStart $ H.srcInfoSpan $ H.ann t2
    , pos <- maybeToList $ between start end "->" ls
    ]


--------------------------------------------------------------------------------
-- | Search for a needle in a haystack of lines. Only part the inside (startRow,
-- startCol), (endRow, endCol) is searched. The return value is the position of
-- the needle.
between :: (Int, Int) -> (Int, Int) -> String -> Lines -> Maybe (Int, Int)
between (startRow, startCol) (endRow, endCol) needle =
    search (startRow, startCol) .
    withLast (take endCol) .
    withHead (drop $ startCol - 1) .
    take (endRow - startRow + 1) .
    drop (startRow - 1)
  where
    withHead _ []       = []
    withHead f (x : xs) = (f x) : xs

    withLast f [x]      = [f x]
    withLast f (x : xs) = x : withLast f xs
    withLast _ []       = []

    search _      []            = Nothing
    search (r, _) ([] : xs)     = search (r + 1, 1) xs
    search (r, c) (x : xs)
        | needle `isPrefixOf` x = Just (r, c)
        | otherwise             = search (r, c + 1) (tail x : xs)


--------------------------------------------------------------------------------
stylish :: Stylish
stylish ls (module', _) = applyChanges changes ls
  where
    changes = replaceAll perLine ls
    perLine = sort $ groupPerLine $ types module' ls
