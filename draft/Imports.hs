import Language.Haskell.Exts.Annotated
import System.Environment (getArgs)
import Data.Ord (comparing)
import Data.List (sortBy)
import Control.Arrow ((&&&))

import Block


--------------------------------------------------------------------------------
imports :: Module l -> [ImportDecl l]
imports (Module _ _ _ is _) = is
imports _                   = []


--------------------------------------------------------------------------------
importName :: ImportDecl l -> String
importName i = let (ModuleName _ n) = importModule i in n


--------------------------------------------------------------------------------
longestImport :: [ImportDecl l] -> Int
longestImport = maximum . map (length . importName)


--------------------------------------------------------------------------------
-- | Groups adjacent imports into larger import blocks
groupAdjacent :: [ImportDecl Block] -> [(Block, [ImportDecl Block])]
groupAdjacent = foldr step []
  where
    -- This code is ugly and not optimal, and no fucks were given.
    step imp is = case break (adjacent b1 . fst) is of
        (_, [])                 -> (b1, [imp]) : is
        (xs, ((b2, imps) : ys)) -> (merge b1 b2, imp : imps) : (xs ++ ys)
      where
        b1 = ann imp


--------------------------------------------------------------------------------
-- | Compare imports for ordering
compareImports :: ImportDecl l -> ImportDecl l -> Ordering
compareImports = comparing (importName &&& importQualified)


--------------------------------------------------------------------------------
main :: IO ()
main = do
    (filePath : _) <- getArgs
    parseResult    <- parseFile filePath
    case parseResult of
        ParseOk x       -> print $
            map fst $ groupAdjacent $ imports $ fmap fromSrcSpanInfo x
        ParseFailed l s ->
            error $ "Parse failed: " ++ show l ++ " " ++ show s
