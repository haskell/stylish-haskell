--------------------------------------------------------------------------------
-- | Simple type that can do replacements on single lines (not spanning,
-- removing or adding lines).
--
-- We want to clean up this interface and possibly extend it a little before we
-- add it to the public API.  In particular, "Replacements" is a horrible name!
module Language.Haskell.Stylish.Replacements
    ( Replacements
    , replace
    , replaceRealSrcSpan
    , apply
    ) where


--------------------------------------------------------------------------------
import qualified Data.Map         as M
import qualified GHC.Types.SrcLoc as GHC


--------------------------------------------------------------------------------
-- Stores spans to change per line.
newtype Replacements = Replacements
    { unReplacements :: M.Map Int [(Int, Int, String)]
    } deriving (Show)


--------------------------------------------------------------------------------
instance Semigroup Replacements where
    Replacements l <> Replacements r = Replacements $ M.unionWith (++) l r


--------------------------------------------------------------------------------
instance Monoid Replacements where
    mempty = Replacements mempty


--------------------------------------------------------------------------------
replaceRealSrcSpan :: GHC.RealSrcSpan -> String -> Replacements
replaceRealSrcSpan rss repl
    | GHC.srcSpanStartLine rss /= GHC.srcSpanEndLine rss = mempty
    | otherwise                                          = replace
        (GHC.srcSpanStartLine rss)
        (GHC.srcSpanStartCol rss)
        (GHC.srcSpanEndCol rss)
        repl


--------------------------------------------------------------------------------
replace :: Int -> Int -> Int -> String -> Replacements
replace line startCol endCol repl
    | startCol > endCol = mempty
    | otherwise         =
        Replacements $ M.singleton line [(startCol, endCol, repl)]


--------------------------------------------------------------------------------
apply :: Replacements -> [String] -> [String]
apply (Replacements repl) ls = do
    (i, l) <- zip [1 ..] ls
    case M.lookup i repl of
        Nothing    -> pure l
        Just repls -> pure $ go repls l
  where
    go [] l = l
    go ((xstart, xend, x) : repls) l =
        let l' = take (xstart - 1) l ++ x ++ drop (xend - 1) l in
        go (adjust (xstart, xend, x) <$> repls) l'

    adjust (xstart, xend, x) (ystart, yend, y)
        | ystart >= xend =
            let offset = length x - (xend - xstart) in
            (ystart + offset, yend + offset, y)
        | otherwise     = (ystart, yend, y)
