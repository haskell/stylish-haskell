{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Step.Imports
    ( Options (..)
    , defaultOptions
    , ImportAlign (..)
    , ListAlign (..)
    , LongListAlign (..)
    , EmptyListAlign (..)
    , ListPadding (..)
    , step
    ) where


--------------------------------------------------------------------------------
import           Control.Arrow                   ((&&&))
import           Control.Monad                   (void)
import qualified Data.Aeson                      as A
import qualified Data.Aeson.Types                as A
import           Data.Char                       (toLower)
import           Data.List                       (intercalate, sortBy)
import qualified Data.Map                        as M
import           Data.Maybe                      (isJust, maybeToList)
import           Data.Ord                        (comparing)
import qualified Data.Set                        as S
import           Data.Semigroup                  (Semigroup ((<>)))
import qualified Language.Haskell.Exts           as H


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Block
import           Language.Haskell.Stylish.Editor
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Util

--------------------------------------------------------------------------------
data Options = Options
    { importAlign    :: ImportAlign
    , listAlign      :: ListAlign
    , padModuleNames :: Bool
    , longListAlign  :: LongListAlign
    , emptyListAlign :: EmptyListAlign
    , listPadding    :: ListPadding
    , separateLists  :: Bool
    , spaceSurround  :: Bool
    } deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options
    { importAlign    = Global
    , listAlign      = AfterAlias
    , padModuleNames = True
    , longListAlign  = Inline
    , emptyListAlign = Inherit
    , listPadding    = LPConstant 4
    , separateLists  = True
    , spaceSurround  = False
    }

data ListPadding
    = LPConstant Int
    | LPModuleName
    deriving (Eq, Show)

data ImportAlign
    = Global
    | File
    | Group
    | None
    deriving (Eq, Show)

data ListAlign
    = NewLine
    | WithModuleName
    | WithAlias
    | AfterAlias
    deriving (Eq, Show)

data EmptyListAlign
    = Inherit
    | RightAfter
    deriving (Eq, Show)

data LongListAlign
    = Inline
    | InlineWithBreak
    | InlineToMultiline
    | Multiline
    deriving (Eq, Show)


--------------------------------------------------------------------------------

modifyImportSpecs :: ([H.ImportSpec l] -> [H.ImportSpec l])
                  -> H.ImportDecl l -> H.ImportDecl l
modifyImportSpecs f imp = imp {H.importSpecs = f' <$> H.importSpecs imp}
  where
    f' (H.ImportSpecList l h specs) = H.ImportSpecList l h (f specs)


--------------------------------------------------------------------------------
imports :: H.Module l -> [H.ImportDecl l]
imports (H.Module _ _ _ is _) = is
imports _                     = []


--------------------------------------------------------------------------------
importName :: H.ImportDecl l -> String
importName i = let (H.ModuleName _ n) = H.importModule i in n

importPackage :: H.ImportDecl l -> Maybe String
importPackage i = H.importPkg i


--------------------------------------------------------------------------------
-- | A "compound import name" is import's name and package (if present). For
-- instance, if you have an import @Foo.Bar@ from package @foobar@, the full
-- name will be @"foobar" Foo.Bar@.
compoundImportName :: H.ImportDecl l -> String
compoundImportName i =
  case importPackage i of
    Nothing  -> importName i
    Just pkg -> show pkg ++ " " ++ importName i


--------------------------------------------------------------------------------
longestImport :: [H.ImportDecl l] -> Int
longestImport = maximum . map (length . compoundImportName)


--------------------------------------------------------------------------------
-- | Compare imports for ordering
compareImports :: H.ImportDecl l -> H.ImportDecl l -> Ordering
compareImports =
  comparing (map toLower . importName &&&
             fmap (map toLower) . importPackage &&&
             H.importQualified)


--------------------------------------------------------------------------------
-- | Remove (or merge) duplicated import specs.
--
-- * When something is mentioned twice, it's removed: @A, A@ -> A
-- * More general forms take priority: @A, A(..)@ -> @A(..)@
-- * Sometimes we have to combine imports: @A(x), A(y)@ -> @A(x, y)@
--
-- Import specs are always sorted by subsequent steps so we don't have to care
-- about preserving order.
deduplicateImportSpecs :: Ord l => H.ImportDecl l -> H.ImportDecl l
deduplicateImportSpecs =
  modifyImportSpecs $
    map recomposeImportSpec .
    M.toList . M.fromListWith (<>) .
    map decomposeImportSpec

-- | What we are importing (variable, class, etc)
data ImportEntity l
  -- | A variable
  = ImportVar l (H.Name l)
  -- | Something that can be imported partially
  | ImportClassOrData l (H.Name l)
  -- | Something else ('H.IAbs')
  | ImportOther l (H.Namespace l) (H.Name l)
  deriving (Eq, Ord)

-- | What we are importing from an 'ImportClassOrData'
data ImportPortion l
  = ImportSome [H.CName l]  -- ^ @A(x, y, z)@
  | ImportAll               -- ^ @A(..)@

instance Ord l => Semigroup (ImportPortion l) where
  ImportSome a <> ImportSome b = ImportSome (setUnion a b)
  _ <> _                       = ImportAll

instance Ord l => Monoid (ImportPortion l) where
  mempty = ImportSome []
  mappend = (<>)

-- | O(n log n) union.
setUnion :: Ord a => [a] -> [a] -> [a]
setUnion a b = S.toList (S.fromList a `S.union` S.fromList b)

decomposeImportSpec :: H.ImportSpec l -> (ImportEntity l, ImportPortion l)
decomposeImportSpec x = case x of
  -- I checked and it looks like namespace's 'l' is always equal to x's 'l'
  H.IAbs l space n -> case space of
    H.NoNamespace      _ -> (ImportClassOrData l n, ImportSome [])
    H.TypeNamespace    _ -> (ImportOther l space n, ImportSome [])
    H.PatternNamespace _ -> (ImportOther l space n, ImportSome [])
  H.IVar l n             -> (ImportVar l n, ImportSome [])
  H.IThingAll l n        -> (ImportClassOrData l n, ImportAll)
  H.IThingWith l n names -> (ImportClassOrData l n, ImportSome names)

recomposeImportSpec :: (ImportEntity l, ImportPortion l) -> H.ImportSpec l
recomposeImportSpec (e, p) = case e of
  ImportClassOrData l n -> case p of
    ImportSome []    -> H.IAbs l (H.NoNamespace l) n
    ImportSome names -> H.IThingWith l n names
    ImportAll        -> H.IThingAll l n
  ImportVar l n         -> H.IVar l n
  ImportOther l space n -> H.IAbs l space n


--------------------------------------------------------------------------------
-- | The implementation is a bit hacky to get proper sorting for input specs:
-- constructors first, followed by functions, and then operators.
compareImportSpecs :: H.ImportSpec l -> H.ImportSpec l -> Ordering
compareImportSpecs = comparing key
  where
    key :: H.ImportSpec l -> (Int, Bool, String)
    key (H.IVar _ x)         = (1, isOperator x, nameToString x)
    key (H.IAbs _ _ x)       = (0, False, nameToString x)
    key (H.IThingAll _ x)    = (0, False, nameToString x)
    key (H.IThingWith _ x _) = (0, False, nameToString x)


--------------------------------------------------------------------------------
-- | Sort the input spec list inside an 'H.ImportDecl'
sortImportSpecs :: H.ImportDecl l -> H.ImportDecl l
sortImportSpecs = modifyImportSpecs (sortBy compareImportSpecs)


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
    Int -> Options -> Bool -> Bool -> Int -> H.ImportDecl l -> [String]
prettyImport columns Options{..} padQualified padName longest imp
    | (void `fmap` H.importSpecs imp) == emptyImportSpec = emptyWrap
    | otherwise = case longListAlign of
        Inline            -> inlineWrap
        InlineWithBreak   -> longListWrapper inlineWrap inlineWithBreakWrap
        InlineToMultiline -> longListWrapper inlineWrap inlineToMultilineWrap
        Multiline         -> longListWrapper inlineWrap multilineWrap
  where
    emptyImportSpec = Just (H.ImportSpecList () False [])
    -- "import" + space + qualifiedLength has space in it.
    listPadding' = listPaddingValue (6 + 1 + qualifiedLength) listPadding
      where
        qualifiedLength =
            if null qualified then 0 else 1 + sum (map length qualified)

    longListWrapper shortWrap longWrap
        | listAlign == NewLine
        || length shortWrap > 1
        || length (head shortWrap) > columns
            = longWrap
        | otherwise = shortWrap

    emptyWrap = case emptyListAlign of
        Inherit    -> inlineWrap
        RightAfter -> [paddedNoSpecBase ++ " ()"]

    inlineWrap = inlineWrapper
        $ mapSpecs
        $ withInit (++ ",")
        . withHead (("(" ++ maybeSpace) ++)
        . withLast (++ (maybeSpace ++ ")"))

    inlineWrapper = case listAlign of
        NewLine        -> (paddedNoSpecBase :) . wrapRest columns listPadding'
        WithModuleName -> wrap columns paddedBase (withModuleNameBaseLength + 4)
        WithAlias      -> wrap columns paddedBase (inlineBaseLength + 1)
        -- Add 1 extra space to ensure same padding as in original code.
        AfterAlias     -> withTail ((' ' : maybeSpace) ++)
            . wrap columns paddedBase (afterAliasBaseLength + 1)

    inlineWithBreakWrap = paddedNoSpecBase : wrapRest columns listPadding'
        ( mapSpecs
        $ withInit (++ ",")
        . withHead (("(" ++ maybeSpace) ++)
        . withLast (++ (maybeSpace ++ ")")))

    inlineToMultilineWrap
        | length inlineWithBreakWrap > 2
        || any ((> columns) . length) (tail inlineWithBreakWrap)
            = multilineWrap
        | otherwise = inlineWithBreakWrap

    -- 'wrapRest 0' ensures that every item of spec list is on new line.
    multilineWrap = paddedNoSpecBase : wrapRest 0 listPadding'
        ( mapSpecs
          ( withHead ("( " ++)
          . withTail (", " ++))
        ++ closer)
      where
        closer = if null importSpecs
            then []
            else [")"]

    paddedBase = base $ padImport $ compoundImportName imp

    paddedNoSpecBase = base $ padImportNoSpec $ compoundImportName imp

    padImport = if hasExtras && padName
        then padRight longest
        else id

    padImportNoSpec = if (isJust (H.importAs imp) || hasHiding) && padName
        then padRight longest
        else id

    base' baseName importAs hasHiding' = unwords $ concat $
        [ ["import"]
        , source
        , safe
        , qualified
        , [baseName]
        , importAs
        , hasHiding'
        ]

    base baseName = base' baseName
        ["as " ++ as | H.ModuleName _ as <- maybeToList $ H.importAs imp]
        ["hiding" | hasHiding]

    inlineBaseLength = length $
                       base' (padImport $ compoundImportName imp) [] []

    withModuleNameBaseLength = length $ base' "" [] []

    afterAliasBaseLength = length $ base' (padImport $ compoundImportName imp)
        ["as " ++ as | H.ModuleName _ as <- maybeToList $ H.importAs imp] []

    (hasHiding, importSpecs) = case H.importSpecs imp of
        Just (H.ImportSpecList _ h l) -> (h, Just l)
        _                             -> (False, Nothing)

    hasExtras = isJust (H.importAs imp) || isJust (H.importSpecs imp)

    qualified
        | H.importQualified imp = ["qualified"]
        | padQualified          =
              if H.importSrc imp
                  then []
                  else if H.importSafe imp
                           then ["    "]
                           else ["         "]
        | otherwise             = []

    safe
        | H.importSafe imp = ["safe"]
        | otherwise        = []

    source
        | H.importSrc imp = ["{-# SOURCE #-}"]
        | otherwise       = []

    mapSpecs f = case importSpecs of
        Nothing -> []     -- Import everything
        Just [] -> ["()"] -- Instance only imports
        Just is -> f $ map (prettyImportSpec separateLists) is

    maybeSpace = case spaceSurround of
        True  -> " "
        False -> ""


--------------------------------------------------------------------------------
prettyImportGroup :: Int -> Options -> Bool -> Int
                  -> [H.ImportDecl LineBlock]
                  -> Lines
prettyImportGroup columns align fileAlign longest imps =
    concatMap (prettyImport columns align padQual padName longest') $
    sortBy compareImports imps
  where
    align' = importAlign align
    padModuleNames' = padModuleNames align

    longest' = case align' of
        Group -> longestImport imps
        _     -> longest

    padName = align' /= None && padModuleNames'

    padQual = case align' of
        Global -> True
        File   -> fileAlign
        Group  -> any H.importQualified imps
        None   -> False


--------------------------------------------------------------------------------
step :: Int -> Options -> Step
step columns = makeStep "Imports" . step' columns


--------------------------------------------------------------------------------
step' :: Int -> Options -> Lines -> Module -> Lines
step' columns align ls (module', _) = applyChanges
    [ change block $ const $
        prettyImportGroup columns align fileAlign longest importGroup
    | (block, importGroup) <- groups
    ]
    ls
  where
    imps    = map (sortImportSpecs . deduplicateImportSpecs) $
              imports $ fmap linesFromSrcSpan module'
    longest = longestImport imps
    groups  = groupAdjacent [(H.ann i, i) | i <- imps]

    fileAlign = case importAlign align of
        File -> any H.importQualified imps
        _    -> False

--------------------------------------------------------------------------------
listPaddingValue :: Int -> ListPadding -> Int
listPaddingValue _ (LPConstant n) = n
listPaddingValue n LPModuleName   = n

--------------------------------------------------------------------------------

instance A.FromJSON ListPadding where
    parseJSON (A.String "module_name") = return LPModuleName
    parseJSON (A.Number n) | n' >= 1   = return $ LPConstant n'
      where
        n' = truncate n
    parseJSON v                        = A.typeMismatch "'module_name' or >=1 number" v
