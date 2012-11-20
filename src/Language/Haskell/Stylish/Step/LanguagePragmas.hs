--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Step.LanguagePragmas
    ( Style (..)
    , step

      -- * Utilities
    , addLanguagePragma
    ) where


--------------------------------------------------------------------------------
import           Data.List                       (intersperse, nub, sort)
import qualified Language.Haskell.Exts.Annotated as H


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Block
import           Language.Haskell.Stylish.Editor
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Util
import           Language.Haskell.Stylish.Wrap


--------------------------------------------------------------------------------
data Style
    = Vertical
    | Compact
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
compactPragmas columns pragmas' = regularWrap columns 13 $
    [Open "{-# LANGUAGE"] ++
    intersperse Comma (map String pragmas') ++
    [Space, Close "#-}"]


--------------------------------------------------------------------------------
prettyPragmas :: Int -> Style -> [String] -> Lines
prettyPragmas _       Vertical = verticalPragmas
prettyPragmas columns Compact  = compactPragmas columns


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
