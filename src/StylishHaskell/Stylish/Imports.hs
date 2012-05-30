--------------------------------------------------------------------------------
module StylishHaskell.Stylish.Imports
    ( stylish
    ) where


--------------------------------------------------------------------------------
import           Control.Arrow                   ((&&&))
import           Data.Char                       (isAlpha)
import           Data.List                       (sortBy)
import           Data.Maybe                      (isJust, maybeToList)
import           Data.Ord                        (comparing)
import qualified Language.Haskell.Exts.Annotated as H


--------------------------------------------------------------------------------
import           StylishHaskell.Block
import           StylishHaskell.Editor
import           StylishHaskell.Stylish
import           StylishHaskell.Util


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
groupAdjacent :: [H.ImportDecl LineBlock]
              -> [(LineBlock, [H.ImportDecl LineBlock])]
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
-- | The implementation is a bit hacky to get proper sorting for input specs:
-- constructors first, followed by functions, and then operators.
compareImportSpecs :: H.ImportSpec l -> H.ImportSpec l -> Ordering
compareImportSpecs = comparing key
  where
    key :: H.ImportSpec l -> (Int, Int, String)
    key (H.IVar _ x)         = let n = nameToString x in (1, operator n, n)
    key (H.IAbs _ x)         = (0, 0, nameToString x)
    key (H.IThingAll _ x)    = (0, 0, nameToString x)
    key (H.IThingWith _ x _) = (0, 0, nameToString x)

    operator []      = 0  -- But this should not happen
    operator (x : _) = if isAlpha x then 0 else 1


--------------------------------------------------------------------------------
-- | Sort the input spec list inside an 'H.ImportDecl'
sortImportSpecs :: H.ImportDecl l -> H.ImportDecl l
sortImportSpecs imp = imp {H.importSpecs = fmap sort $ H.importSpecs imp}
  where
    sort (H.ImportSpecList l h specs) = H.ImportSpecList l h $
        sortBy compareImportSpecs specs


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
prettyImportGroup :: Int -> [H.ImportDecl LineBlock] -> Lines
prettyImportGroup longest = map (prettyImport longest) . sortBy compareImports


--------------------------------------------------------------------------------
stylish :: Stylish
stylish ls (module', _) = flip applyChanges ls
    [ change block (prettyImportGroup longest importGroup)
    | (block, importGroup) <- groups
    ]
  where
    imps    = map sortImportSpecs $ imports $ fmap linesFromSrcSpan module'
    longest = longestImport imps
    groups  = groupAdjacent imps
