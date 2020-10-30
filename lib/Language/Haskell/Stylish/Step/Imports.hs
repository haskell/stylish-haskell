{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Language.Haskell.Stylish.Step.Imports
  ( Options (..)
  , defaultOptions
  , ImportAlign (..)
  , ListAlign (..)
  , LongListAlign (..)
  , EmptyListAlign (..)
  , ListPadding (..)
  , step

  , printImport
  ) where

--------------------------------------------------------------------------------
import           Control.Monad                   (forM_, when, void)
import           Data.Function                   ((&), on)
import           Data.Functor                    (($>))
import           Data.Foldable                   (toList)
import           Data.Maybe                      (isJust)
import           Data.List                       (sortBy)
import           Data.List.NonEmpty              (NonEmpty(..))
import qualified Data.List.NonEmpty              as NonEmpty
import qualified Data.Map                        as Map
import qualified Data.Set                        as Set


--------------------------------------------------------------------------------
import           BasicTypes                      (StringLiteral (..),
                                                  SourceText (..))
import qualified FastString                      as FS
import           GHC.Hs.Extension                (GhcPs)
import qualified GHC.Hs.Extension                as GHC
import           GHC.Hs.ImpExp
import           Module                          (moduleNameString)
import           RdrName                         (RdrName)
import           SrcLoc                          (Located, GenLocated(..), unLoc)


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Block
import           Language.Haskell.Stylish.Module
import           Language.Haskell.Stylish.Ordering
import           Language.Haskell.Stylish.Printer
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Editor
import           Language.Haskell.Stylish.GHC
import           Language.Haskell.Stylish.Util


--------------------------------------------------------------------------------
data Options = Options
    { importAlign     :: ImportAlign
    , listAlign       :: ListAlign
    , padModuleNames  :: Bool
    , longListAlign   :: LongListAlign
    , emptyListAlign  :: EmptyListAlign
    , listPadding     :: ListPadding
    , separateLists   :: Bool
    , spaceSurround   :: Bool
    } deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options
    { importAlign     = Global
    , listAlign       = AfterAlias
    , padModuleNames  = True
    , longListAlign   = Inline
    , emptyListAlign  = Inherit
    , listPadding     = LPConstant 4
    , separateLists   = True
    , spaceSurround   = False
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
    | Repeat
    deriving (Eq, Show)

data EmptyListAlign
    = Inherit
    | RightAfter
    deriving (Eq, Show)

data LongListAlign
    = Inline -- inline
    | InlineWithBreak -- new_line
    | InlineToMultiline -- new_line_multiline
    | Multiline -- multiline
    deriving (Eq, Show)


--------------------------------------------------------------------------------
step :: Maybe Int -> Options -> Step
step columns = makeStep "Imports (ghc-lib-parser)" . printImports columns


--------------------------------------------------------------------------------
printImports :: Maybe Int -> Options -> Lines -> Module -> Lines
printImports maxCols align ls m = applyChanges changes ls
  where
    groups = moduleImportGroups m
    moduleStats = foldMap importStats . fmap unLoc $ concatMap toList groups
    changes = do
        group <- groups
        pure $ formatGroup maxCols align m moduleStats group

formatGroup
    :: Maybe Int -> Options -> Module -> ImportStats
    -> NonEmpty (Located Import) -> Change String
formatGroup maxCols options m moduleStats imports =
    let newLines = formatImports maxCols options m moduleStats imports in
    change (importBlock imports) (const newLines)

importBlock :: NonEmpty (Located a) -> Block String
importBlock group = Block
    (getStartLineUnsafe $ NonEmpty.head group)
    (getEndLineUnsafe   $ NonEmpty.last group)

formatImports
    :: Maybe Int    -- ^ Max columns.
    -> Options      -- ^ Options.
    -> Module       -- ^ Module.
    -> ImportStats  -- ^ Module stats.
    -> NonEmpty (Located Import) -> Lines
formatImports maxCols options m moduleStats rawGroup =
  runPrinter_ (PrinterConfig maxCols) [] m do
  let 
     
    group
      = NonEmpty.sortWith unLocated rawGroup
      & mergeImports

    unLocatedGroup = fmap unLocated $ toList group

    align' = importAlign options
    padModuleNames' = padModuleNames options
    padNames = align' /= None && padModuleNames'

    stats = case align' of
        Global -> moduleStats {isAnyQualified = True}
        File   -> moduleStats
        Group  -> foldMap importStats unLocatedGroup
        None   -> mempty

  forM_ group \imp -> printQualified options padNames stats imp >> newline

--------------------------------------------------------------------------------
printQualified :: Options -> Bool -> ImportStats -> Located Import -> P ()
printQualified Options{..} padNames stats (L _ decl) = do
  let decl' = rawImport decl

  putText "import" >> space

  case (isSource decl, isAnySource stats) of
    (True, _) -> putText "{-# SOURCE #-}" >> space
    (_, True) -> putText "              " >> space
    _         -> pure ()

  when (isSafe decl) (putText "safe" >> space)

  case (isQualified decl, isAnyQualified stats) of
    (True, _) -> putText "qualified" >> space
    (_, True) -> putText "         " >> space
    _         -> pure ()

  moduleNamePosition <- length <$> getCurrentLine
  forM_ (ideclPkgQual decl') $ \pkg -> putText (stringLiteral pkg) >> space
  putText (moduleName decl)

  -- Only print spaces if something follows.
  when padNames $
    when (isJust (ideclAs decl') || isHiding decl ||
            not (null $ ideclHiding decl')) $
      putText $
        replicate (isLongestImport stats - importModuleNameLength decl) ' '

  beforeAliasPosition <- length <$> getCurrentLine
  forM_ (ideclAs decl') \(L _ name) ->
    space >> putText "as" >> space >> putText (moduleNameString name)
  afterAliasPosition <- length <$> getCurrentLine

  when (isHiding decl) (space >> putText "hiding")

  let putOffset = putText $ replicate offset ' '
      offset = case listPadding of
        LPConstant n -> n
        LPModuleName -> moduleNamePosition

  case snd <$> ideclHiding decl' of
    Nothing            -> pure ()
    Just (L _ [])      -> case emptyListAlign of
      RightAfter -> modifyCurrentLine trimRight >> space >> putText "()"
      Inherit -> case listAlign of
        NewLine ->
          modifyCurrentLine trimRight >> newline >> putOffset >> putText "()"
        _ -> space >> putText "()"
    Just (L _ imports) -> do
      let printedImports = flagEnds $ -- [P ()]
            fmap ((printImport separateLists) . unLocated)
            (prepareImportList imports)

      -- Since we might need to output the import module name several times, we
      -- need to save it to a variable:
      wrapPrefix <- case listAlign of
        AfterAlias -> pure $ replicate (afterAliasPosition + 1) ' '
        WithAlias -> pure $ replicate (beforeAliasPosition + 1) ' '
        Repeat -> fmap (++ " (") getCurrentLine
        WithModuleName -> pure $ replicate (moduleNamePosition + offset) ' '
        NewLine -> pure $ replicate offset ' '

      let -- Helper
          doSpaceSurround = when spaceSurround space

          -- Try to put everything on one line.
          printAsSingleLine = forM_ printedImports $ \(imp, start, end) -> do
            when start $ putText "(" >> doSpaceSurround
            imp
            if end then doSpaceSurround >> putText ")" else comma >> space

          -- Try to put everything one by one, wrapping if that fails.
          printAsInlineWrapping wprefix = forM_ printedImports $
            \(imp, start, end) ->
            patchForRepeatHiding $ wrapping
              (do
                if start then putText "(" >> doSpaceSurround else space
                imp
                if end then doSpaceSurround >> putText ")" else comma)
              (do
                case listAlign of
                    -- In 'Repeat' mode, end lines with ')' rather than ','.
                    Repeat | not start -> modifyCurrentLine . withLast $
                        \c -> if c == ',' then ')' else c
                    _ | start && spaceSurround ->
                        -- Only necessary if spaceSurround is enabled.
                        modifyCurrentLine trimRight
                    _ -> pure ()
                newline
                void wprefix
                case listAlign of
                  -- '(' already included in repeat
                  Repeat         -> pure ()
                  -- Print the much needed '('
                  _ | start      -> putText "(" >> doSpaceSurround
                  -- Don't bother aligning if we're not in inline mode.
                  _ | longListAlign /= Inline -> pure ()
                  -- 'Inline + AfterAlias' is really where we want to be careful
                  -- with spacing.
                  AfterAlias -> space >> doSpaceSurround
                  WithModuleName -> pure ()
                  WithAlias -> pure ()
                  NewLine -> pure ()
                imp
                if end then doSpaceSurround >> putText ")" else comma)

          -- Put everything on a separate line.  'spaceSurround' can be
          -- ignored.
          printAsMultiLine = forM_ printedImports $ \(imp, start, end) -> do
            when start $ modifyCurrentLine trimRight  -- We added some spaces.
            newline
            putOffset
            if start then putText "( " else putText ", "
            imp
            when end $ newline >> putOffset >> putText ")"

      case longListAlign of
        Multiline -> wrapping
          (space >> printAsSingleLine)
          printAsMultiLine
        Inline | NewLine <- listAlign -> do
          modifyCurrentLine trimRight
          newline >> putOffset >> printAsInlineWrapping (putText wrapPrefix)
        Inline -> space >> printAsInlineWrapping (putText wrapPrefix)
        InlineWithBreak -> wrapping
          (space >> printAsSingleLine)
          (do
            modifyCurrentLine trimRight
            newline >> putOffset >> printAsInlineWrapping putOffset)
        InlineToMultiline -> wrapping
          (space >> printAsSingleLine)
          (wrapping
            (do
              modifyCurrentLine trimRight
              newline >> putOffset >> printAsSingleLine)
            printAsMultiLine)
  where
    -- We cannot wrap/repeat 'hiding' imports since then we would get multiple
    -- imports hiding different things.
    patchForRepeatHiding = case listAlign of
        Repeat | isHiding decl -> withColumns Nothing
        _                      -> id


--------------------------------------------------------------------------------
printImport :: Bool -> IE GhcPs -> P ()
printImport _ (IEVar _ name) = do
    printIeWrappedName name
printImport _ (IEThingAbs _ name) = do
    printIeWrappedName name
printImport separateLists (IEThingAll _ name) = do
    printIeWrappedName name
    when separateLists space
    putText "(..)"
printImport _ (IEModuleContents _ (L _ m)) = do
    putText "module"
    space
    putText (moduleNameString m)
printImport separateLists (IEThingWith _ name wildcard imps _) = do
    printIeWrappedName name
    when separateLists space
    let ellipsis = case wildcard of
          IEWildcard _position -> [putText ".."]
          NoIEWildcard         -> []
    parenthesize $
      sep (comma >> space) (ellipsis <> fmap printIeWrappedName imps)
printImport _ (IEGroup _ _ _ ) =
    error "Language.Haskell.Stylish.Printer.Imports.printImportExport: unhandled case 'IEGroup'"
printImport _ (IEDoc _ _) =
    error "Language.Haskell.Stylish.Printer.Imports.printImportExport: unhandled case 'IEDoc'"
printImport _ (IEDocNamed _ _) =
    error "Language.Haskell.Stylish.Printer.Imports.printImportExport: unhandled case 'IEDocNamed'"
printImport _ (XIE ext) =
    GHC.noExtCon ext


--------------------------------------------------------------------------------
printIeWrappedName :: LIEWrappedName RdrName -> P ()
printIeWrappedName lie = unLocated lie & \case
  IEName n -> putRdrName n
  IEPattern n -> putText "pattern" >> space >> putRdrName n
  IEType n -> putText "type" >> space >> putRdrName n

mergeImports :: NonEmpty (Located Import) -> NonEmpty (Located Import)
mergeImports (x :| []) = x :| []
mergeImports (h :| (t : ts))
  | canMergeImport (unLocated h) (unLocated t) = mergeImports (mergeModuleImport h t :| ts)
  | otherwise = h :| mergeImportsTail (t : ts)
  where
    mergeImportsTail (x : y : ys)
      | canMergeImport (unLocated x) (unLocated y) = mergeImportsTail ((mergeModuleImport x y) : ys)
      | otherwise = x : mergeImportsTail (y : ys)
    mergeImportsTail xs = xs

moduleName :: Import -> String
moduleName
  = moduleNameString
  . unLocated
  . ideclName
  . rawImport


--------------------------------------------------------------------------------
data ImportStats = ImportStats
    { isLongestImport :: !Int
    , isAnySource     :: !Bool
    , isAnyQualified  :: !Bool
    , isAnySafe       :: !Bool
    }

instance Semigroup ImportStats where
    l <> r = ImportStats
        { isLongestImport = isLongestImport l `max` isLongestImport r
        , isAnySource     = isAnySource     l ||    isAnySource     r
        , isAnyQualified  = isAnyQualified  l ||    isAnyQualified  r
        , isAnySafe       = isAnySafe       l ||    isAnySafe       r
        }

instance Monoid ImportStats where
    mappend = (<>)
    mempty  = ImportStats 0 False False False

importStats :: Import -> ImportStats
importStats i =
    ImportStats (importModuleNameLength i) (isSource i) (isQualified i) (isSafe i)

-- Computes length till module name, includes package name.
-- TODO: this should reuse code with the printer
importModuleNameLength :: Import -> Int
importModuleNameLength imp =
    (case ideclPkgQual (rawImport imp) of
        Nothing -> 0
        Just sl -> 1 + length (stringLiteral sl)) +
    (length $ moduleName imp)


--------------------------------------------------------------------------------
stringLiteral :: StringLiteral -> String
stringLiteral sl = case sl_st sl of
    NoSourceText -> FS.unpackFS $ sl_fs sl
    SourceText s -> s


--------------------------------------------------------------------------------
isQualified :: Import -> Bool
isQualified
  = (/=) NotQualified
  . ideclQualified
  . rawImport

isHiding :: Import -> Bool
isHiding
  = maybe False fst
  . ideclHiding
  . rawImport

isSource :: Import -> Bool
isSource
  = ideclSource
  . rawImport

isSafe :: Import -> Bool
isSafe
  = ideclSafe
  . rawImport

--------------------------------------------------------------------------------
-- | Cleans up an import item list.
--
-- * Sorts import items.
-- * Sort inner import lists, e.g. `import Control.Monad (Monad (return, join))`
-- * Removes duplicates from import lists.
prepareImportList :: [LIE GhcPs] -> [LIE GhcPs]
prepareImportList =
  sortBy compareLIE . map (fmap prepareInner) .
  concatMap (toList . snd) . Map.toAscList . mergeByName
 where
  mergeByName :: [LIE GhcPs] -> Map.Map RdrName (NonEmpty (LIE GhcPs))
  mergeByName imports0 = Map.fromListWith
    -- Note that ideally every NonEmpty will just have a single entry and we
    -- will be able to merge everything into that entry.  Exotic imports can
    -- mess this up, though.  So they end up in the tail of the list.
    (\(x :| xs) (y :| ys) -> case ieMerge (unLocated x) (unLocated y) of
      Just z -> (x $> z) :| (xs ++ ys)  -- Keep source from `x`
      Nothing -> x :| (xs ++ y : ys))
    [(ieName $ unLocated imp, imp :| []) | imp <- imports0]

  prepareInner :: IE GhcPs -> IE GhcPs
  prepareInner = \case
    -- Simplify `A ()` to `A`.
    IEThingWith x n NoIEWildcard [] [] -> IEThingAbs x n
    IEThingWith x n w ns fs ->
      IEThingWith x n w (sortBy (compareWrappedName `on` unLoc) ns) fs
    ie -> ie

  -- Merge two import items, assuming they have the same name.
  ieMerge :: IE GhcPs -> IE GhcPs -> Maybe (IE GhcPs)
  ieMerge l@(IEVar _ _)      _                  = Just l
  ieMerge _                  r@(IEVar _ _)      = Just r
  ieMerge (IEThingAbs _ _)   r                  = Just r
  ieMerge l                  (IEThingAbs _ _)   = Just l
  ieMerge l@(IEThingAll _ _) _                  = Just l
  ieMerge _                  r@(IEThingAll _ _) = Just r
  ieMerge (IEThingWith x0 n0 w0 ns0 []) (IEThingWith _ _ w1 ns1 [])
    | w0 /= w1  = Nothing
    | otherwise = Just $
        -- TODO: sort the `ns0 ++ ns1`?
        IEThingWith x0 n0 w0 (nubOn (unwrapName . unLoc) $ ns0 ++ ns1) []
  ieMerge _ _ = Nothing


--------------------------------------------------------------------------------
nubOn :: Ord k => (a -> k) -> [a] -> [a]
nubOn f = go Set.empty
 where
  go _   []              = []
  go acc (x : xs)
    | y `Set.member` acc = go acc xs
    | otherwise          = x : go (Set.insert y acc) xs
   where
    y = f x
