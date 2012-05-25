--------------------------------------------------------------------------------
module StylishHaskell.Block
    ( Lines

    , Block (..)
    , blockLength
    , fromSrcSpanInfo
    , moveBlock
    , adjacent
    , merge
    , overlapping
    ) where


--------------------------------------------------------------------------------
import           Control.Arrow                   (arr, (>>>), (&&&))
import qualified Language.Haskell.Exts.Annotated as H


--------------------------------------------------------------------------------
type Lines = [String]


--------------------------------------------------------------------------------
-- | Indicates a line span
data Block = Block
    { blockStart :: Int
    , blockEnd   :: Int
    } deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
blockLength :: Block -> Int
blockLength (Block start end) = end - start + 1


--------------------------------------------------------------------------------
fromSrcSpanInfo :: H.SrcSpanInfo -> Block
fromSrcSpanInfo = H.srcInfoSpan >>>
    H.srcSpanStartLine &&& H.srcSpanEndLine >>>
    arr (uncurry Block)


--------------------------------------------------------------------------------
moveBlock :: Int -> Block -> Block
moveBlock offset (Block start end) = Block (start + offset) (end + offset)


--------------------------------------------------------------------------------
adjacent :: Block -> Block -> Bool
adjacent b1 b2 = follows b1 b2 || follows b2 b1
  where
    follows (Block _ e1) (Block s2 _) = e1 + 1 == s2


--------------------------------------------------------------------------------
merge :: Block -> Block -> Block
merge (Block s1 e1) (Block s2 e2) = Block (min s1 s2) (max e1 e2)


--------------------------------------------------------------------------------
overlapping :: [Block] -> Bool
overlapping blocks =
    any (uncurry overlapping') $ zip blocks (drop 1 blocks)
  where
    overlapping' (Block _ e1) (Block s2 _) = e1 >= s2
