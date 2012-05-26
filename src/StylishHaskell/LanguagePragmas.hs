--------------------------------------------------------------------------------
module StylishHaskell.LanguagePragmas where


--------------------------------------------------------------------------------
import           Data.List                       (intercalate, nub, sort)
import qualified Language.Haskell.Exts.Annotated as H


--------------------------------------------------------------------------------
import           StylishHaskell.Block
import           StylishHaskell.Editor
import           StylishHaskell.Parse


--------------------------------------------------------------------------------
pragmas :: H.Module l -> [(l, [String])]
pragmas (H.Module _ _ ps _ _) =
    [(l, map nameToString names) | H.LanguagePragma l names <- ps]
pragmas _                     = []


--------------------------------------------------------------------------------
-- | TODO: put this in utilities?
nameToString :: H.Name l -> String
nameToString (H.Ident _ str)  = str
nameToString (H.Symbol _ str) = str


--------------------------------------------------------------------------------
-- | The start of the first block
firstLocation :: [(Block, [String])] -> Int
firstLocation = minimum . map (blockStart . fst)


--------------------------------------------------------------------------------
-- | TODO: multiple lines if longer than 80 columns
prettyPragmas :: [String] -> Lines
prettyPragmas pragmas' =
    ["{-# LANGUAGE " ++ intercalate ", " pragmas' ++ " #-}"]


--------------------------------------------------------------------------------
stylish :: Lines -> Module -> Lines
stylish ls (module', _)
    | null pragmas' = ls
    | otherwise     = applyChanges changes ls
  where
    pragmas' = pragmas $ fmap fromSrcSpanInfo module'
    deletes  = map (delete . fst) pragmas'
    uniques  = nub $ sort $ concatMap snd pragmas'
    loc      = firstLocation pragmas'
    changes  = insert loc (prettyPragmas uniques) : deletes
