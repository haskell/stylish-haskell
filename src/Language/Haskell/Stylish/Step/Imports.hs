--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Step.Imports
    ( Align (..)
    , step
    ) where


--------------------------------------------------------------------------------
import           Control.Arrow                   ((&&&))
import           Data.Char                       (isAlpha, toLower)
import           Data.List                       (intercalate, sortBy)
import           Data.Maybe                      (isJust, maybeToList)
import           Data.Ord                        (comparing)
import qualified Language.Haskell.Exts.Annotated as H


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Block
import           Language.Haskell.Stylish.Editor
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Util


--------------------------------------------------------------------------------
data Align
    = Global
    | File
    | Group
    | None
    deriving (Eq, Show)


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
-- | Compare imports for ordering
compareImports :: H.ImportDecl l -> H.ImportDecl l -> Ordering
compareImports = comparing (map toLower . importName &&& H.importQualified)


--------------------------------------------------------------------------------
-- | The implementation is a bit hacky to get proper sorting for input specs:
-- constructors first, followed by functions, and then operators.
compareImportSpecs :: H.ImportSpec l -> H.ImportSpec l -> Ordering
compareImportSpecs = comparing key
  where
    key :: H.ImportSpec l -> (Int, Int, String)
    key (H.IVar _ x)         = let n = nameToString x in (1, operator n, n)
    key (H.IAbs _ _ x)       = (0, 0, nameToString x)
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
-- | By default, haskell-src-exts pretty-prints
--
-- > import Foo (Bar(..))
--
-- but we want
--
-- > import Foo (Bar (..))
--
-- instead.
prettyImportSpec :: H.ImportSpec l -> String
prettyImportSpec (H.IThingAll  _ n)     = H.prettyPrint n ++ " (..)"
prettyImportSpec (H.IThingWith _ n cns) = H.prettyPrint n ++ " (" ++
    intercalate ", " (map H.prettyPrint cns) ++ ")"
prettyImportSpec x                      = H.prettyPrint x


--------------------------------------------------------------------------------
prettyImport :: Int -> Bool -> Bool -> Int -> H.ImportDecl l -> [String]
prettyImport columns padQualified padName longest imp =
    wrap columns base (length base + 2) $
    (if hiding then ("hiding" :) else id) $
    case importSpecs of
        Nothing -> []     -- Import everything
        Just [] -> ["()"] -- Instance only imports
        Just is ->
            withInit (++ ",") $
            withHead ("(" ++) $
            withLast (++ ")") $
            map prettyImportSpec $
            is
  where
    base = unwords $ concat
         [ ["import"]
         , qualified
         , (fmap show $ maybeToList $ H.importPkg imp)
         , [(if hasExtras && padName then padRight longest else id)
            (importName imp)]
         , ["as " ++ as | H.ModuleName _ as <- maybeToList $ H.importAs imp]
         ]

    (hiding, importSpecs) = case H.importSpecs imp of
        Just (H.ImportSpecList _ h l) -> (h, Just l)
        _                             -> (False, Nothing)

    hasExtras = isJust (H.importAs imp) || isJust (H.importSpecs imp)

    qualified
        | H.importQualified imp = ["qualified"]
        | padQualified          = ["         "]
        | otherwise             = []


--------------------------------------------------------------------------------
prettyImportGroup :: Int -> Align -> Bool -> Int -> [H.ImportDecl LineBlock]
                  -> Lines
prettyImportGroup columns align fileAlign longest imps =
    concatMap (prettyImport columns padQual padName longest') $
    sortBy compareImports imps
  where
    longest' = case align of
        Group -> longestImport imps
        _     -> longest

    padName = align /= None

    padQual = case align of
        Global -> True
        File   -> fileAlign
        Group  -> any H.importQualified imps
        None   -> False


--------------------------------------------------------------------------------
step :: Int -> Align -> Step
step columns = makeStep "Imports" . step' columns


--------------------------------------------------------------------------------
step' :: Int -> Align -> Lines -> Module -> Lines
step' columns align ls (module', _) = flip applyChanges ls
    [ change block $ const $
        prettyImportGroup columns align fileAlign longest importGroup
    | (block, importGroup) <- groups
    ]
  where
    imps    = map sortImportSpecs $ imports $ fmap linesFromSrcSpan module'
    longest = longestImport imps
    groups  = groupAdjacent [(H.ann i, i) | i <- imps]

    fileAlign = case align of
        File -> any H.importQualified imps
        _    -> False
