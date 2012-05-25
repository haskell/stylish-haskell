import Control.Applicative ((<$>))
import Language.Haskell.Exts.Annotated
import System.Environment (getArgs)
import Data.Ord (comparing)
import Data.Maybe (isJust, maybeToList)
import Data.List (sortBy)
import Control.Arrow ((&&&))


--------------------------------------------------------------------------------
import Block
import qualified Block
import qualified Block as Herp


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
prettyImport :: Int -> ImportDecl l -> String
prettyImport longest imp = unwords $ concat
    [ ["import"]
    , [if importQualified imp then "qualified" else "         "]
    , [(if hasExtras then padRight longest else id) (importName imp)]
    , ["as " ++ as | ModuleName _ as <- maybeToList $ importAs imp]
    , [prettyPrint specs | specs <- maybeToList $ importSpecs imp]
    ]
  where
    hasExtras = isJust (importAs imp) || isJust (importSpecs imp)


--------------------------------------------------------------------------------
prettyImportGroup :: Int -> [ImportDecl Block] -> Lines
prettyImportGroup longest = map (prettyImport longest) . sortBy compareImports


--------------------------------------------------------------------------------
padRight :: Int -> String -> String
padRight len str = str ++ replicate (len - length str) ' '


--------------------------------------------------------------------------------
changes :: Module Block -> [Change]
changes module' =
    [ change block (prettyImportGroup longest importGroup)
    | (block, importGroup) <- groups
    ]
  where
    imps    = imports module'
    longest = longestImport imps
    groups  = groupAdjacent imps


--------------------------------------------------------------------------------
main :: IO ()
main = do
    (filePath : _) <- getArgs
    parseResult    <- parseFile filePath
    contents       <- lines <$> readFile filePath
    case parseResult of
        ParseOk x       -> do
            let module' = fmap fromSrcSpanInfo x
            putStr $ unlines $ makeChanges (changes module') contents
        ParseFailed l s ->
            error $ "Parse failed: " ++ show l ++ " " ++ show s
