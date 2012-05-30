--------------------------------------------------------------------------------
module StylishHaskell.LanguagePragmas
    ( stylish
    ) where


--------------------------------------------------------------------------------
import           Data.List                       (nub, sort)
import qualified Language.Haskell.Exts.Annotated as H


--------------------------------------------------------------------------------
import           StylishHaskell.Block
import           StylishHaskell.Editor
import           StylishHaskell.Stylish
import           StylishHaskell.Util


--------------------------------------------------------------------------------
pragmas :: H.Module l -> [(l, [String])]
pragmas (H.Module _ _ ps _ _) =
    [(l, map nameToString names) | H.LanguagePragma l names <- ps]
pragmas _                     = []


--------------------------------------------------------------------------------
-- | The start of the first block
firstLocation :: [(Block a, [String])] -> Int
firstLocation = minimum . map (blockStart . fst)


--------------------------------------------------------------------------------
-- | TODO: multiple lines if longer than 80 columns
prettyPragmas :: [String] -> Lines
prettyPragmas pragmas' =
    [ "{-# LANGUAGE " ++ padRight longest pragma ++ " #-}"
    | pragma <- pragmas'
    ]
  where
    longest = maximum $ map length pragmas'


--------------------------------------------------------------------------------
stylish :: Stylish
stylish ls (module', _)
    | null pragmas' = ls
    | otherwise     = applyChanges changes ls
  where
    pragmas' = pragmas $ fmap fromSrcSpanInfo module'
    deletes  = map (delete . fst) pragmas'
    uniques  = nub $ sort $ concatMap snd pragmas'
    loc      = firstLocation pragmas'
    changes  = insert loc (prettyPragmas uniques) : deletes
