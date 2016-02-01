{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Step.Imports
    ( Align (..)
    , ImportAlign (..)
    , ListAlign (..)
    , LongListAlign (..)
    , step
    ) where


--------------------------------------------------------------------------------
import           Control.Arrow                   ((&&&))
import           Data.Char                       (toLower)
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
data Align = Align
    { importAlign   :: ImportAlign
    , listAlign     :: ListAlign
    , longListAlign :: LongListAlign
    , listPadding   :: Int
    , separateLists :: Bool
    }
    deriving (Eq, Show)

data ImportAlign
    = Global
    | File
    | Group
    | None
    deriving (Eq, Show)

data ListAlign
    = NewLine
    | WithAlias
    | AfterAlias
    deriving (Eq, Show)

data LongListAlign
    = Inline
    | InlineWithBreak
    | InlineToMultiline
    | Multiline
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
    key :: H.ImportSpec l -> (Int, Bool, String)
    key (H.IVar _ x)       = (1, isOperator x, nameToString x)
    key (H.IAbs _ _ x)         = (0, False, nameToString x)
    key (H.IThingAll _ x)    = (0, False, nameToString x)
    key (H.IThingWith _ x _) = (0, False, nameToString x)


--------------------------------------------------------------------------------
-- | Sort the input spec list inside an 'H.ImportDecl'
sortImportSpecs :: H.ImportDecl l -> H.ImportDecl l
sortImportSpecs imp = imp {H.importSpecs = sort' <$> H.importSpecs imp}
  where
    sort' (H.ImportSpecList l h specs) = H.ImportSpecList l h $
        sortBy compareImportSpecs specs


--------------------------------------------------------------------------------
-- | Order of imports in sublist is:
-- Constructors, accessors/methods, operators.
compareImportSubSpecs :: H.CName l -> H.CName l -> Ordering
compareImportSubSpecs = comparing key
  where
    key :: H.CName l -> (Int, Bool, String)
    key (H.ConName _ x) = (0, False,        nameToString x)
    key (H.VarName _ x) = (1, isOperator x, nameToString x)


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
prettyImportSpec :: (Ord l) => Bool -> H.ImportSpec l -> String
prettyImportSpec separate = prettyImportSpec'
  where
    prettyImportSpec' (H.IThingAll  _ n)     = H.prettyPrint n ++ sep "(..)"
    prettyImportSpec' (H.IThingWith _ n cns) = H.prettyPrint n
        ++ sep "("
        ++ intercalate ", "
          (map H.prettyPrint $ sortBy compareImportSubSpecs cns)
        ++ ")"
    prettyImportSpec' x                      = H.prettyPrint x

    sep = if separate then (' ' :) else id


--------------------------------------------------------------------------------
prettyImport :: (Ord l, Show l) =>
    Int -> Align -> Bool -> Bool -> Int -> H.ImportDecl l -> [String]
prettyImport columns Align{..} padQualified padName longest imp =
    case longListAlign of
        Inline -> inlineWrap
        InlineWithBreak -> longListWrapper inlineWrap inlineWithBreakWrap
        InlineToMultiline -> longListWrapper inlineWrap inlineToMultilineWrap
        Multiline -> longListWrapper inlineWrap multilineWrap
  where
    longListWrapper shortWrap longWrap
        | listAlign == NewLine
        || length shortWrap > 1
        || length (head shortWrap) > columns
            = longWrap
        | otherwise = shortWrap

    inlineWrap = inlineWrapper
        $ mapSpecs
        $ withInit (++ ",")
        . withHead ("(" ++)
        . withLast (++ ")")

    inlineWrapper = case listAlign of
        NewLine    -> (paddedNoSpecBase :) . wrapRest columns listPadding
        WithAlias  -> wrap columns paddedBase (inlineBaseLength + 1)
        -- Add 1 extra space to ensure same padding as in original code.
        AfterAlias -> withTail (' ' :)
            . wrap columns paddedBase (afterAliasBaseLength + 1)

    inlineWithBreakWrap = paddedNoSpecBase : wrapRest columns listPadding
        ( mapSpecs
        $ withInit (++ ",")
        . withHead ("(" ++)
        . withLast (++ ")"))

    inlineToMultilineWrap
        | length inlineWithBreakWrap > 2
        || any ((> columns) . length) (tail inlineWithBreakWrap)
            = multilineWrap
        | otherwise = inlineWithBreakWrap

    -- 'wrapRest 0' ensures that every item of spec list is on new line.
    multilineWrap = paddedNoSpecBase : wrapRest 0 listPadding
        ( mapSpecs
          ( withHead ("( " ++)
          . withTail (", " ++))
        ++ [")"])

    paddedBase = base $ padImport $ importName imp

    paddedNoSpecBase = base $ padImportNoSpec $ importName imp

    padImport = if hasExtras && padName
        then padRight longest
        else id

    padImportNoSpec = if (isJust (H.importAs imp) || hasHiding) && padName
        then padRight longest
        else id

    base' baseName importAs hasHiding' = unwords $ concat $ filter (not . null)
        [ ["import"]
        , qualified
        , show <$> maybeToList (H.importPkg imp)
        , [baseName]
        , importAs
        , hasHiding'
        ]

    base baseName = base' baseName
        ["as " ++ as | H.ModuleName _ as <- maybeToList $ H.importAs imp]
        ["hiding" | hasHiding]

    inlineBaseLength = length $ base' (padImport $ importName imp) [] []

    afterAliasBaseLength = length $ base' (padImport $ importName imp)
        ["as " ++ as | H.ModuleName _ as <- maybeToList $ H.importAs imp] []

    (hasHiding, importSpecs) = case H.importSpecs imp of
        Just (H.ImportSpecList _ h l) -> (h, Just l)
        _                             -> (False, Nothing)

    hasExtras = isJust (H.importAs imp) || isJust (H.importSpecs imp)

    qualified
        | H.importQualified imp = ["qualified"]
        | padQualified          = ["         "]
        | otherwise             = []

    mapSpecs f = case importSpecs of
        Nothing -> []     -- Import everything
        Just [] -> ["()"] -- Instance only imports
        Just is -> f $ map (prettyImportSpec separateLists) is


--------------------------------------------------------------------------------
prettyImportGroup :: Int -> Align -> Bool -> Int
                  -> [H.ImportDecl LineBlock]
                  -> Lines
prettyImportGroup columns align fileAlign longest imps =
    concatMap (prettyImport columns align padQual padName longest') $
    sortBy compareImports imps
  where
    align' = importAlign align

    longest' = case align' of
        Group -> longestImport imps
        _     -> longest

    padName = align' /= None

    padQual = case align' of
        Global -> True
        File   -> fileAlign
        Group  -> any H.importQualified imps
        None   -> False


--------------------------------------------------------------------------------
step :: Int -> Align -> Step
step columns = makeStep "Imports" . step' columns


--------------------------------------------------------------------------------
step' :: Int -> Align -> Lines -> Module -> Lines
step' columns align ls (module', _) = applyChanges
    [ change block $ const $
        prettyImportGroup columns align fileAlign longest importGroup
    | (block, importGroup) <- groups
    ]
    ls
  where
    imps    = map sortImportSpecs $ imports $ fmap linesFromSrcSpan module'
    longest = longestImport imps
    groups  = groupAdjacent [(H.ann i, i) | i <- imps]

    fileAlign = case importAlign align of
        File -> any H.importQualified imps
        _    -> False
