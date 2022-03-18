{-# LANGUAGE LambdaCase #-}

--------------------------------------------------------------------------------
-- | This module provides you with a line-based editor. It's main feature is
-- that you can specify multiple changes at the same time, e.g.:
--
-- > [deleteLine 3, changeLine 4 ["Foo"]]
--
-- when this is evaluated, we take into account that 4th line will become the
-- 3rd line before it needs changing.
module Language.Haskell.Stylish.Editor
    ( module Language.Haskell.Stylish.Block

    , Edits
    , apply

    , replace
    , replaceRealSrcSpan
    , changeLine
    , changeLines
    , insertLines
    ) where


--------------------------------------------------------------------------------
import qualified Data.Map                       as M
import           Data.Maybe                     (fromMaybe)
import qualified GHC.Types.SrcLoc               as GHC


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Block


--------------------------------------------------------------------------------
data Change
    -- | Insert some lines.
    = CInsert [String]
    -- | Replace the block of N lines by the given lines.
    | CBlock Int ([String] -> [String])
    -- | Replace (startCol, endCol) by the given string on this line.
    | CLine Int Int String


--------------------------------------------------------------------------------
-- | Due to the function in CBlock we cannot write a lawful Ord instance, but
-- this lets us merge-sort changes.
beforeChange :: Change -> Change -> Bool
beforeChange (CInsert _)   _             = True
beforeChange _             (CInsert _)   = False
beforeChange (CBlock _ _)  _             = True
beforeChange _             (CBlock _ _)  = False
beforeChange (CLine x _ _) (CLine y _ _) = x <= y


--------------------------------------------------------------------------------
prettyChange :: Int -> Change -> String
prettyChange l (CInsert ls) =
    show l ++ " insert " ++ show (length ls) ++ " lines"
prettyChange l (CBlock n _) = show l ++ "-" ++ show (l + n) ++ " replace lines"
prettyChange l (CLine start end x) =
    show l ++ ":" ++ show start ++ "-" ++ show end ++ " replace by " ++ show x


--------------------------------------------------------------------------------
-- | Merge in order
mergeChanges :: [Change] -> [Change] -> [Change]
mergeChanges = go
  where
    go []       ys       = ys
    go xs       []       = xs
    go (x : xs) (y : ys) =
        if x `beforeChange` y then x : go xs (y : ys) else y : go (x : xs) ys


--------------------------------------------------------------------------------
-- Stores sorted spans to change per line.
newtype Edits = Edits {unEdits :: M.Map Int [Change]}


--------------------------------------------------------------------------------
instance Show Edits where
    show edits = unlines $ do
        (line, changes) <- M.toAscList $ unEdits edits
        prettyChange line <$> changes


--------------------------------------------------------------------------------
instance Semigroup Edits where
    Edits l <> Edits r = Edits $ M.unionWith mergeChanges l r


--------------------------------------------------------------------------------
instance Monoid Edits where
    mempty = Edits mempty


--------------------------------------------------------------------------------
replaceRealSrcSpan :: GHC.RealSrcSpan -> String -> Edits
replaceRealSrcSpan rss repl
    | GHC.srcSpanStartLine rss /= GHC.srcSpanEndLine rss = mempty
    | otherwise                                          = replace
        (GHC.srcSpanStartLine rss)
        (GHC.srcSpanStartCol rss)
        (GHC.srcSpanEndCol rss)
        repl


--------------------------------------------------------------------------------
replace :: Int -> Int -> Int -> String -> Edits
replace line startCol endCol repl
    | startCol > endCol = mempty
    | otherwise         =
        Edits $ M.singleton line [CLine startCol endCol repl]


--------------------------------------------------------------------------------
changeLine :: Int -> (String -> [String]) -> Edits
changeLine start f = changeLines (Block start start) $ \ls -> case ls of
    l : _ -> f l
    _     -> f ""


--------------------------------------------------------------------------------
changeLines :: Block String -> ([String] -> [String]) -> Edits
changeLines (Block start end) f =
    Edits $ M.singleton start [CBlock (end - start + 1) f]


--------------------------------------------------------------------------------
insertLines :: Int -> [String] -> Edits
insertLines line ls = Edits $ M.singleton line [CInsert ls]


--------------------------------------------------------------------------------
data Conflict = Conflict Int Change Int Change


--------------------------------------------------------------------------------
prettyConflict :: Conflict -> String
prettyConflict (Conflict l1 c1 l2 c2) = unlines
    [ "Conflict between edits:"
    , "- " ++ prettyChange l1 c1
    , "- " ++ prettyChange l2 c2
    ]


--------------------------------------------------------------------------------
conflicts :: Edits -> [Conflict]
conflicts (Edits edits) = M.toAscList edits >>= uncurry checkChanges
  where
    checkChanges _ [] = []
    checkChanges i (CInsert _ : cs) = checkChanges i cs
    checkChanges i (c1@(CBlock _ _) : c2 : _) = [Conflict i c1 i c2]
    checkChanges i [c1@(CBlock n _)] = do
        i' <- [i + 1 .. i + n - 1]
        case M.lookup i' edits of
            Just (c2 : _) -> [Conflict i c1 i' c2]
            _             -> []
    checkChanges i (c1@(CLine xstart xend _) : c2@(CLine ystart _ _) : cs)
        | xstart == ystart = [Conflict i c1 i c2]
        | xend > ystart    = [Conflict i c1 i c2]
        | otherwise        = checkChanges i (c2 : cs)
    checkChanges _ (CLine _ _ _ : _) = []


--------------------------------------------------------------------------------
apply :: Edits -> [String] -> [String]
apply (Edits edits) = case conflicts (Edits edits) of
    c : _ -> error $ "Language.Haskell.Stylish.Editor: " ++ prettyConflict c
    _     -> go 1 (editsFor 1)
  where
    editsFor i = fromMaybe [] $ M.lookup i edits

    go _ _ [] = []
    go i [] (l : ls) = l : go (i + 1) (editsFor $ i + 1) ls
    go i (CInsert ls' : cs) ls = ls' ++ go i cs ls
    go i (CBlock n f : _cs) ls =
        let (domain, ls') = splitAt n ls in
        f domain ++ go (i + n) (editsFor $ i + n) ls'
    go i (CLine xstart xend x : cs) (l : ls) =
        let l' = take (xstart - 1) l ++ x ++ drop (xend - 1) l in
        go i (adjust xstart xend x <$> cs) (l' : ls)

    adjust _ _ _ (CInsert xs) = CInsert xs
    adjust _ _ _ (CBlock n f) = CBlock n f
    adjust xstart xend x (CLine ystart yend y)
        | ystart >= xend =
            let offset = length x - (xend - xstart) in
            CLine (ystart + offset) (yend + offset) y
        | otherwise     = CLine ystart yend y
