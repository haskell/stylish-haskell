--------------------------------------------------------------------------------
-- module StylishHaskell.Stylish.UnicodeSyntax where


--------------------------------------------------------------------------------
import qualified Language.Haskell.Exts.Annotated as H
import System.Environment
import Data.Maybe (maybeToList)
import Data.List (isPrefixOf)


--------------------------------------------------------------------------------
import StylishHaskell.Stylish
import StylishHaskell.Util
import StylishHaskell.Parse


--------------------------------------------------------------------------------
types :: H.Module H.SrcSpanInfo -> Lines -> [(Int, Int)]
types module' ls =
    [ pos
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
main :: IO ()
main = do
    (filePath : _) <- getArgs
    contents       <- readFile filePath
    case parseModule (Just filePath) contents of
        Left err           -> error err
        Right (module', _) -> print $ types module' (lines contents)
