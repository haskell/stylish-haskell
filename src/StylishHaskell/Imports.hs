--------------------------------------------------------------------------------
module StylishHaskell.Imports
    ( stylish
    ) where


--------------------------------------------------------------------------------
import           Control.Arrow                   ((&&&))
import           Data.List                       (sortBy)
import           Data.Maybe                      (isJust, maybeToList)
import           Data.Ord                        (comparing)
import qualified Language.Haskell.Exts.Annotated as H


--------------------------------------------------------------------------------
import           StylishHaskell.Block
import           StylishHaskell.Editor
import           StylishHaskell.Parse


--------------------------------------------------------------------------------
imports :: H.Module l -> [H.ImportDecl l]
imports (H.Module _ _ _ is _) = is
imports _                     = []


--------------------------------------------------------------------------------
importName :: H.ImportDecl l -> String
importName i = let (H.ModuleName _ n) = H.importModule i in n


--------------------------------------------------------------------------------
longestImport :: [H.ImportDecl l] -> Int
longestImport = maximum . map (length . importName)


--------------------------------------------------------------------------------
-- | Groups adjacent imports into larger import blocks
groupAdjacent :: [H.ImportDecl Block] -> [(Block, [H.ImportDecl Block])]
groupAdjacent = foldr step []
  where
    -- This code is ugly and not optimal, and no fucks were given.
    step imp is = case break (adjacent b1 . fst) is of
        (_, [])                 -> (b1, [imp]) : is
        (xs, ((b2, imps) : ys)) -> (merge b1 b2, imp : imps) : (xs ++ ys)
      where
        b1 = H.ann imp


--------------------------------------------------------------------------------
-- | Compare imports for ordering
compareImports :: H.ImportDecl l -> H.ImportDecl l -> Ordering
compareImports = comparing (importName &&& H.importQualified)


--------------------------------------------------------------------------------
prettyImport :: Int -> H.ImportDecl l -> String
prettyImport longest imp = unwords $ concat
    [ ["import"]
    , [if H.importQualified imp then "qualified" else "         "]
    , [(if hasExtras then padRight longest else id) (importName imp)]
    , ["as " ++ as | H.ModuleName _ as <- maybeToList $ H.importAs imp]
    , [H.prettyPrint specs | specs <- maybeToList $ H.importSpecs imp]
    ]
  where
    hasExtras = isJust (H.importAs imp) || isJust (H.importSpecs imp)


--------------------------------------------------------------------------------
prettyImportGroup :: Int -> [H.ImportDecl Block] -> Lines
prettyImportGroup longest = map (prettyImport longest) . sortBy compareImports


--------------------------------------------------------------------------------
padRight :: Int -> String -> String
padRight len str = str ++ replicate (len - length str) ' '


--------------------------------------------------------------------------------
stylish :: Lines -> Module -> Lines
stylish ls (module', _) = flip applyChanges ls
    [ change block (prettyImportGroup longest importGroup)
    | (block, importGroup) <- groups
    ]
  where
    imps    = imports $ fmap fromSrcSpanInfo module'
    longest = longestImport imps
    groups  = groupAdjacent imps
