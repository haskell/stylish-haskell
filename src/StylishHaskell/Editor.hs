--------------------------------------------------------------------------------
-- | This module provides you with a line-based editor. It's main feature is
-- that you can specify multiple changes at the same time, e.g.:
--
-- > [deleteLine 3, changeLine 4 ["Foo"]]
--
-- when this is evaluated, we take into account that 4th line will become the
-- 3rd line before it needs changing.
module StylishHaskell.Editor
    ( Change
    , applyChanges

    , change
    , changeLine
    , delete
    , deleteLine
    , insert
    ) where


--------------------------------------------------------------------------------
import           StylishHaskell.Block


--------------------------------------------------------------------------------
-- | Changes the lines indicated by the 'Block' into the given 'Lines'
data Change a = Change
    { changeBlock :: Block
    , changeLines :: [a]
    } deriving (Eq, Show)


--------------------------------------------------------------------------------
moveChange :: Int -> Change a -> Change a
moveChange offset (Change block ls) = Change (moveBlock offset block) ls


--------------------------------------------------------------------------------
-- | Number of additional lines introduced when a change is made.
changeExtraLines :: Change a -> Int
changeExtraLines (Change block ls) = length ls - blockLength block


--------------------------------------------------------------------------------
applyChanges :: [Change a] -> [a] -> [a]
applyChanges changes
    | overlapping blocks = error $
        "StylishHaskell.Editor.applyChanges: " ++
        "refusing to make overlapping changes"
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
        let block      = changeBlock ch
            (pre, ls') = splitAt (blockStart block - n) ls
            (_, post)  = splitAt (blockLength block) ls'
            extraLines = changeExtraLines ch
            chs'       = map (moveChange extraLines) chs
            n'         = blockStart block + blockLength block + extraLines
        in pre ++ (changeLines ch) ++ go n' chs' post


--------------------------------------------------------------------------------
-- | Change a block of lines for some other lines
change :: Block -> [a] -> Change a
change = Change


--------------------------------------------------------------------------------
-- | Change a single line for some other lines
changeLine :: Int -> [a] -> Change a
changeLine start = change (Block start start)


--------------------------------------------------------------------------------
-- | Delete a block of lines
delete :: Block -> Change a
delete block = Change block []


--------------------------------------------------------------------------------
-- | Delete a single line
deleteLine :: Int -> Change a
deleteLine start = delete (Block start start)


--------------------------------------------------------------------------------
-- | Insert something /before/ the given lines
insert :: Int -> [a] -> Change a
insert start = Change (Block start (start - 1))
