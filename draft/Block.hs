module Block
    ( Lines

    , Block (..)
    , blockLength
    , fromSrcSpanInfo
    , moveBlock
    , adjacent
    , merge
    , overlapping
    
    , Change (..)
    , moveChange
    , changeExtraLines
    , makeChanges

    , change
    , changeLine
    , delete
    , deleteLine
    , insert
    ) where


--------------------------------------------------------------------------------
import           Control.Arrow                   (arr, (>>>), (&&&))
import           Language.Haskell.Exts.Annotated


--------------------------------------------------------------------------------
type Lines = [String]


--------------------------------------------------------------------------------
-- Indicates a line span
data Block = Block
    { blockStart :: Int
    , blockEnd   :: Int
    } deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
blockLength :: Block -> Int
blockLength (Block start end) = end - start + 1


--------------------------------------------------------------------------------
fromSrcSpanInfo :: SrcSpanInfo -> Block
fromSrcSpanInfo = srcInfoSpan >>>
    srcSpanStartLine &&& srcSpanEndLine >>>
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


--------------------------------------------------------------------------------
-- | Changes the lines indicated by the 'Block' into the given 'Lines'
data Change = Change
    { changeBlock :: Block
    , changeLines :: Lines
    } deriving (Eq, Show)


--------------------------------------------------------------------------------
moveChange :: Int -> Change -> Change
moveChange offset (Change block ls) = Change (moveBlock offset block) ls


--------------------------------------------------------------------------------
-- | Number of additional lines introduced when a change is made.
changeExtraLines :: Change -> Int
changeExtraLines (Change block ls) = length ls - blockLength block


--------------------------------------------------------------------------------
makeChanges :: [Change] -> Lines -> Lines
makeChanges changes
    | overlapping blocks = error
        "Block.makeChanges: refusing to make overlapping changes"
    | otherwise          = go 1 changes
  where
    blocks = map changeBlock changes

    go _ []                ls = ls
    go n (ch : chs) ls =
        -- Divide the remaining lines into:
        --
        -- > pre
        -- > old  (lines that are affected by the change)
        -- > post
        --
        -- And generate:
        --
        -- > pre
        -- > new
        -- > (recurse)
        --
        let Change block new = ch
            (pre, ls') = splitAt (blockStart block - n) ls
            (_, post)  = splitAt (blockLength block) ls'
            extraLines = changeExtraLines ch
            chs'       = map (moveChange extraLines) chs
            n'         = blockStart block + blockLength block + extraLines
        in pre ++ new ++ go n' chs' post


--------------------------------------------------------------------------------
-- | Change a block of lines for some other lines
change :: Block -> Lines -> Change
change = Change


--------------------------------------------------------------------------------
-- | Change a single line for some other lines
changeLine :: Int -> Lines -> Change
changeLine start = change (Block start start)


--------------------------------------------------------------------------------
-- | Delete a block of lines
delete :: Block -> Change
delete block = Change block []


--------------------------------------------------------------------------------
-- | Delete a single line
deleteLine :: Int -> Change
deleteLine start = delete (Block start start)


--------------------------------------------------------------------------------
-- | Insert something /before/ the given lines
insert :: Int -> Lines -> Change
insert start = Change (Block start (start - 1))
