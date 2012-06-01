--------------------------------------------------------------------------------
module StylishHaskell.Stylish.LanguagePragmas
    ( stylish

      -- * Utilities
    , addLanguagePragma
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
    pragmas' = pragmas $ fmap linesFromSrcSpan module'
    deletes  = map (delete . fst) pragmas'
    uniques  = nub $ sort $ concatMap snd pragmas'
    loc      = firstLocation pragmas'
    changes  = insert loc (prettyPragmas uniques) : deletes


--------------------------------------------------------------------------------
-- | Add a LANGUAGE pragma to a module if it is not present already.
addLanguagePragma :: String -> H.Module H.SrcSpanInfo -> [Change String]
addLanguagePragma pragma modu
    | pragma `elem` present = []
    | otherwise             = [insert line ["{-# LANGUAGE " ++ pragma ++ "#-}"]]
  where
    pragmas' = pragmas (fmap linesFromSrcSpan modu)
    present  = concatMap snd pragmas'
    line     = if null pragmas' then 1 else firstLocation pragmas'
