--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Step.LanguagePragmas
    ( Style (..)
    , step

      -- * Utilities
    , addLanguagePragma
    ) where


--------------------------------------------------------------------------------
import           Data.List                       (nub, sort)
import qualified Language.Haskell.Exts.Annotated as H


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Block
import           Language.Haskell.Stylish.Editor
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Util


--------------------------------------------------------------------------------
data Style
    = Vertical
    | Compact
    | Line
    deriving (Eq, Show)


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
verticalPragmas :: [String] -> Lines
verticalPragmas pragmas' =
    [ "{-# LANGUAGE " ++ padRight longest pragma ++ " #-}"
    | pragma <- pragmas'
    ]
  where
    longest = maximum $ map length pragmas'


--------------------------------------------------------------------------------
compactPragmas :: Int -> [String] -> Lines
compactPragmas columns pragmas' = wrap columns "{-# LANGUAGE" 13 $
    map (++ ",") (init pragmas') ++ [last pragmas', "#-}"]


--------------------------------------------------------------------------------
linePragmas :: Int -> [String] -> Lines
linePragmas _ [] = []
linePragmas columns (p:pragmas') =
  let (ls, curr, _) = foldl step ([], p, length p) pragmas'
      ps = ls ++ [curr]
      longest = maximum $ map length ps
  in map (wrapLANGUAGE . padRight longest) ps
  where
    maxWidth = columns - 17
    step (ls, curr, width) str
      | width' > maxWidth = (ls ++ [curr], str, len)
      | otherwise         = (ls, curr ++ ", " ++ str, width')
      where
        len = length str
        width' = width + 2 + len
    wrapLANGUAGE ps = "{-# LANGUAGE " ++ ps ++  " #-}"


--------------------------------------------------------------------------------
prettyPragmas :: Int -> Style -> [String] -> Lines
prettyPragmas _       Vertical = verticalPragmas
prettyPragmas columns Compact  = compactPragmas columns
prettyPragmas columns Line     = linePragmas    columns


--------------------------------------------------------------------------------
step :: Int -> Style -> Bool -> Step
step columns style = makeStep "LanguagePragmas" . step' columns style


--------------------------------------------------------------------------------
step' :: Int -> Style -> Bool -> Lines -> Module -> Lines
step' columns style removeRedundant ls (module', _)
    | null pragmas' = ls
    | otherwise     = applyChanges changes ls
  where
    filterRedundant
        | removeRedundant = filter (not . isRedundant module')
        | otherwise       = id

    pragmas' = pragmas $ fmap linesFromSrcSpan module'
    uniques  = filterRedundant $ nub $ sort $ snd =<< pragmas'
    loc      = firstLocation pragmas'
    deletes  = map (delete . fst) pragmas'
    changes  = insert loc (prettyPragmas columns style uniques) : deletes


--------------------------------------------------------------------------------
-- | Add a LANGUAGE pragma to a module if it is not present already.
addLanguagePragma :: String -> H.Module H.SrcSpanInfo -> [Change String]
addLanguagePragma prag modu
    | prag `elem` present = []
    | otherwise           = [insert line ["{-# LANGUAGE " ++ prag ++ " #-}"]]
  where
    pragmas' = pragmas (fmap linesFromSrcSpan modu)
    present  = concatMap snd pragmas'
    line     = if null pragmas' then 1 else firstLocation pragmas'


--------------------------------------------------------------------------------
-- | Check if a language pragma is redundant. We can't do this for all pragmas,
-- but we do a best effort.
isRedundant :: H.Module H.SrcSpanInfo -> String -> Bool
isRedundant m "ViewPatterns" = isRedundantViewPatterns m
isRedundant m "BangPatterns" = isRedundantBangPatterns m
isRedundant _ _              = False


--------------------------------------------------------------------------------
-- | Check if the ViewPatterns language pragma is redundant.
isRedundantViewPatterns :: H.Module H.SrcSpanInfo -> Bool
isRedundantViewPatterns m = null
    [() | H.PViewPat _ _ _ <- everything m :: [H.Pat H.SrcSpanInfo]]


--------------------------------------------------------------------------------
-- | Check if the BangPatterns language pragma is redundant.
isRedundantBangPatterns :: H.Module H.SrcSpanInfo -> Bool
isRedundantBangPatterns m = null
    [() | H.PBangPat _ _ <- everything m :: [H.Pat H.SrcSpanInfo]]
