{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE DoAndIfThenElse   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Language.Haskell.Stylish.Step.Imports
  ( Options (..)
  , defaultOptions
  , ImportAlign (..)
  , ListAlign (..)
  , LongListAlign (..)
  , EmptyListAlign (..)
  , ListPadding (..)
  , GroupRule (..)
  , step

  , printImport

  , parsePattern
  , unsafeParsePattern
  ) where

--------------------------------------------------------------------------------
import           Control.Monad                     (forM_, void, when)
import qualified Data.Aeson                        as A
import           Data.Foldable                     (toList)
import           Data.Function                     (on, (&))
import           Data.Functor                      (($>))
import           Data.List                         (groupBy, intercalate,
                                                    partition, sortBy, sortOn)
import           Data.List.NonEmpty                (NonEmpty (..))
import qualified Data.List.NonEmpty                as NonEmpty
import qualified Data.Map                          as Map
import           Data.Maybe                        (fromMaybe, isJust, mapMaybe)
import           Data.Sequence                     (Seq ((:|>)))
import qualified Data.Sequence                     as Seq
import qualified Data.Set                          as Set
import qualified Data.Text                         as T
import qualified GHC.Data.FastString               as GHC
import qualified GHC.Hs                            as GHC
import qualified GHC.Types.Name.Reader             as GHC
import qualified GHC.Types.SourceText              as GHC
import qualified GHC.Types.SrcLoc                  as GHC
import qualified GHC.Unit.Module.Name              as GHC
import qualified GHC.Unit.Types                    as GHC
import           Text.Regex.TDFA                   (Regex)
import qualified Text.Regex.TDFA                   as Regex
import           Text.Regex.TDFA.ReadRegex         (parseRegex)

--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Block
import qualified Language.Haskell.Stylish.Editor   as Editor
import           Language.Haskell.Stylish.Module
import           Language.Haskell.Stylish.Ordering
import           Language.Haskell.Stylish.Printer
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
    , postQualified  :: Bool
    , groupImports   :: Bool
    , groupRules     :: [GroupRule]
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
    , postQualified  = False
    , groupImports   = False
    , groupRules     = [defaultGroupRule]
    }
  where defaultGroupRule = GroupRule
          { match    = unsafeParsePattern ".*"
          , subGroup = Just $ unsafeParsePattern "^[^.]+"
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

-- | A rule for grouping imports that specifies which module names
-- belong in a group and (optionally) how to break them up into
-- sub-groups.
--
-- See the documentation for the group_rules setting in
-- data/stylish-haskell.yaml for more details.
data GroupRule = GroupRule
  { match    :: Pattern
    -- ^ The pattern that determines whether a rule applies to a
    -- module name.
  , subGroup :: Maybe Pattern
    -- ^ An optional pattern for breaking the group up into smaller
    -- sub-groups.
  } deriving (Show, Eq)

instance A.FromJSON GroupRule where
  parseJSON = A.withObject "group_rule" parse
    where parse o = GroupRule
                <$> (o A..: "match")
                <*> (o A..:? "sub_group")

-- | A compiled regular expression. Provides instances that 'Regex'
-- does not have (eg 'Show', 'Eq' and 'FromJSON').
--
-- Construct with 'parsePattern' to maintain the invariant that
-- 'string' is the exact regex string used to compile 'regex'.
data Pattern = Pattern
  { regex  :: Regex
    -- ^ The compiled regular expression.
  , string :: String
    -- ^ The valid regex string that 'regex' was compiled from.
  }

instance Show Pattern where show = show . string

instance Eq Pattern where (==) = (==) `on` string

instance A.FromJSON Pattern where
  parseJSON = A.withText "regex" parse
    where parse text = case parsePattern $ T.unpack text of
            Left err  -> fail $ "Invalid regex:\n" <> err
            Right pat -> pure pat


-- | Parse a string into a compiled regular expression ('Pattern').
--
-- Returns a human-readable parse error message if the string is not
-- valid regex syntax.
--
-- >>> parsePattern "^([^.]+)"
-- Right "^([^.]+)"
--
-- >>> parsePattern "("
-- Left "\"(\" (line 1, column 2):\nunexpected end of input\nexpecting empty () or anchor ^ or $ or an atom"
parsePattern :: String -> Either String Pattern
parsePattern string = case parseRegex string of
  Right _  -> Right $ Pattern { string, regex = Regex.makeRegex string }
  Left err -> Left (show err)

-- | Parse a string into a regular expression, raising a runtime
-- exception if the string is not valid regex syntax.
--
-- >>> unsafeParsePattern "^([^.]+)"
-- "^([^.]+)"
--
-- >>> unsafeParsePattern "("
-- "*** Exception: "(" (line 1, column 2):
-- unexpected end of input
-- expecting empty () or anchor ^ or $ or an atom
unsafeParsePattern :: String -> Pattern
unsafeParsePattern = either error id . parsePattern

--------------------------------------------------------------------------------
step :: Maybe Int -> Options -> Step
step columns = makeStep "Imports (ghc-lib-parser)" . printImports columns


--------------------------------------------------------------------------------
printImports :: Maybe Int -> Options -> Lines -> Module -> Lines
printImports maxCols options ls m = Editor.apply changes ls
  where
    groups = moduleImportGroups m
    moduleStats = foldMap importStats . fmap GHC.unLoc $ concatMap toList groups
    changes
      | groupImports options =
          groupAndFormat maxCols options moduleStats groups
      | otherwise =
          foldMap (formatGroup maxCols options moduleStats) groups

formatGroup
    :: Maybe Int -> Options -> ImportStats
    -> NonEmpty (GHC.LImportDecl GHC.GhcPs) -> Editor.Edits
formatGroup maxCols options moduleStats imports =
    let newLines = formatImports maxCols options moduleStats imports in
    Editor.changeLines (importBlock imports) (const newLines)

importBlock :: NonEmpty (GHC.LImportDecl GHC.GhcPs)  -> Block String
importBlock group = Block
    (GHC.srcSpanStartLine . src $ NonEmpty.head group)
    (GHC.srcSpanEndLine   . src $ NonEmpty.last group)
  where
    src = fromMaybe (error "importBlock: missing location") .
        GHC.srcSpanToRealSrcSpan . GHC.getLocA

formatImports
    :: Maybe Int    -- ^ Max columns.
    -> Options      -- ^ Options.
    -> ImportStats  -- ^ Module stats.
    -> NonEmpty (GHC.LImportDecl GHC.GhcPs) -> Lines
formatImports maxCols options moduleStats rawGroup =
  runPrinter_ (PrinterConfig maxCols) do
  let
    group :: NonEmpty (GHC.LImportDecl GHC.GhcPs)
    group
      = NonEmpty.sortBy (compareImports `on` GHC.unLoc) rawGroup
      & mergeImports

    unLocatedGroup = fmap GHC.unLoc $ toList group

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
-- | Reorganize imports into groups based on 'groupPatterns', then
-- format each group as specified by the rest of 'Options'.
--
-- Note: this will discard blank lines and comments inside the imports
-- section.
groupAndFormat
  :: Maybe Int
  -> Options
  -> ImportStats
  -> [NonEmpty (GHC.LImportDecl GHC.GhcPs)]
  -> Editor.Edits
groupAndFormat _ _ _ [] = mempty
groupAndFormat maxCols options moduleStats groups =
  Editor.changeLines block (const regroupedLines)
  where
    regroupedLines :: Lines
    regroupedLines = intercalate [""] $
      map (formatImports maxCols options moduleStats) grouped

    grouped :: [NonEmpty (GHC.LImportDecl GHC.GhcPs)]
    grouped = groupByRules (groupRules options) imports

    imports :: [GHC.LImportDecl GHC.GhcPs]
    imports = concatMap toList groups

    -- groups is non-empty by the pattern for this case
    -- imports is non-empty as long as groups is non-empty
    block = Block
      (GHC.srcSpanStartLine . src $ head imports)
      (GHC.srcSpanEndLine   . src $ last imports)
    src = fromMaybe (error "regroupImports: missing location") .
      GHC.srcSpanToRealSrcSpan . GHC.getLocA

-- | Group imports based on a list of patterns.
--
-- See the documentation for @group_patterns@ in
-- @data/stylish-haskell.yaml@ for details about the patterns and
-- grouping logic.
groupByRules
  :: [GroupRule]
  -- ^ The patterns specifying the groups to build. Order matters:
  -- earlier patterns take precedence over later ones.
  -> [GHC.LImportDecl GHC.GhcPs]
  -- ^ The imports to group. Order does not matter.
  -> [NonEmpty (GHC.LImportDecl GHC.GhcPs)]
groupByRules rules allImports = toList $ go rules allImports Seq.empty
  where
    go :: [GroupRule]
       -> [GHC.LImportDecl GHC.GhcPs]
       -> Seq (NonEmpty (GHC.LImportDecl GHC.GhcPs))
       -> Seq (NonEmpty (GHC.LImportDecl GHC.GhcPs))
    go [] [] groups            = groups
    go [] imports groups       = groups :|> NonEmpty.fromList imports
    go (r : rs) imports groups =
      let
        (groups', rest) = extract r imports
      in
        go rs rest (groups <> groups')

    extract :: GroupRule
            -> [GHC.LImportDecl GHC.GhcPs]
            -> ( Seq (NonEmpty (GHC.LImportDecl GHC.GhcPs))
               , [GHC.LImportDecl GHC.GhcPs]
               )
    extract GroupRule { match, subGroup } imports =
      let
        (matched, rest) = partition (matches match) imports
        subgroups = groupBy ((==) `on` firstMatch subGroup) $
                      sortOn (firstMatch subGroup) matched
      in
        -- groupBy never produces empty groups, so this mapMaybe will
        -- not discard anything from subgroups
        (Seq.fromList $ mapMaybe NonEmpty.nonEmpty subgroups, rest)

    matches :: Pattern -> GHC.LImportDecl GHC.GhcPs -> Bool
    matches Pattern { regex } import_ = Regex.match regex $ moduleName import_

    firstMatch :: Maybe Pattern -> GHC.LImportDecl GHC.GhcPs -> String
    firstMatch (Just Pattern { regex }) import_ =
      Regex.match regex $ moduleName import_
    firstMatch Nothing _ =
      "" -- constant grouping key, so everything will be grouped together

    moduleName = importModuleName . GHC.unLoc


--------------------------------------------------------------------------------
printQualified
    :: Options -> Bool -> ImportStats -> GHC.LImportDecl GHC.GhcPs -> P ()
printQualified Options{..} padNames stats ldecl = do
    putText "import" >> space

    case (isSource decl, isAnySource stats) of
      (True, _) -> putText "{-# SOURCE #-}" >> space
      (_, True) -> putText "              " >> space
      _         -> pure ()

    when (GHC.ideclSafe decl) (putText "safe" >> space)

    let module_ = do
            moduleNamePosition <- length <$> getCurrentLine
            forM_ (GHC.ideclPkgQual decl) $ \pkg ->
                putText (stringLiteral pkg) >> space
            putText (importModuleName decl)

            -- Only print spaces if something follows.
            let somethingFollows =
                    isJust (GHC.ideclAs decl) || isHiding decl ||
                    not (null $ GHC.ideclHiding decl)
            when (padNames && somethingFollows) $ putText $ replicate
                (isLongestImport stats - importModuleNameLength decl)
                ' '
            pure moduleNamePosition

    moduleNamePosition <-
        case (postQualified, isQualified decl, isAnyQualified stats) of
            (False, True , _   ) -> putText "qualified" *> space *> module_
            (False, _    , True) -> putText "         " *> space *> module_
            (True , True , _   ) -> module_ <* space <* putText "qualified"
            _                    -> module_

    beforeAliasPosition <- length <$> getCurrentLine
    forM_ (GHC.ideclAs decl) $ \lname -> do
        space >> putText "as" >> space
        putText . GHC.moduleNameString $ GHC.unLoc lname

    afterAliasPosition <- length <$> getCurrentLine

    when (isHiding decl) (space >> putText "hiding")

    let putOffset = putText $ replicate offset ' '
        offset = case listPadding of
            LPConstant n -> n
            LPModuleName -> moduleNamePosition

    pure ()

    case snd <$> GHC.ideclHiding decl of
        Nothing -> pure ()
        Just limports | null (GHC.unLoc limports) -> case emptyListAlign of
            RightAfter -> modifyCurrentLine trimRight >> space >> putText "()"
            Inherit -> case listAlign of
                NewLine -> do
                    modifyCurrentLine trimRight
                    newline >> putOffset >> putText "()"
                _ -> space >> putText "()"

        Just limports -> do
            let imports = GHC.unLoc limports
                printedImports = flagEnds $ -- [P ()]
                    (printImport separateLists) . GHC.unLoc <$>
                    prepareImportList imports

            -- Since we might need to output the import module name several times, we
            -- need to save it to a variable:
            wrapPrefix <- case listAlign of
                AfterAlias -> pure $ replicate (afterAliasPosition + 1) ' '
                WithAlias -> pure $ replicate (beforeAliasPosition + 1) ' '
                Repeat -> fmap (++ " (") getCurrentLine
                WithModuleName -> pure $ replicate (moduleNamePosition + offset) ' '
                NewLine -> pure $ replicate offset ' '

            -- Helper
            let doSpaceSurround = when spaceSurround space

            -- Try to put everything on one line.
            let printAsSingleLine = forM_ printedImports $ \(imp, start, end) -> do
                    when start $ putText "(" >> doSpaceSurround
                    imp
                    if end then doSpaceSurround >> putText ")" else comma >> space

            -- Try to put everything one by one, wrapping if that fails.
            let printAsInlineWrapping wprefix = forM_ printedImports $
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
            let printAsMultiLine = forM_ printedImports $ \(imp, start, end) -> do
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
    decl = GHC.unLoc ldecl

    -- We cannot wrap/repeat 'hiding' imports since then we would get multiple
    -- imports hiding different things.
    patchForRepeatHiding = case listAlign of
        Repeat | isHiding decl -> withColumns Nothing
        _                      -> id


--------------------------------------------------------------------------------
printImport :: Bool -> GHC.IE GHC.GhcPs -> P ()
printImport _ (GHC.IEVar _ name) = do
    printIeWrappedName name
printImport _ (GHC.IEThingAbs _ name) = do
    printIeWrappedName name
printImport separateLists (GHC.IEThingAll _ name) = do
    printIeWrappedName name
    when separateLists space
    putText "(..)"
printImport _ (GHC.IEModuleContents _ modu) = do
    putText "module"
    space
    putText . GHC.moduleNameString $ GHC.unLoc modu
printImport separateLists (GHC.IEThingWith _ name wildcard imps) = do
    printIeWrappedName name
    when separateLists space
    let ellipsis = case wildcard of
          GHC.IEWildcard _position -> [putText ".."]
          GHC.NoIEWildcard         -> []
    parenthesize $
      sep (comma >> space) (ellipsis <> fmap printIeWrappedName imps)
printImport _ (GHC.IEGroup _ _ _ ) =
    error "Language.Haskell.Stylish.Printer.Imports.printImportExport: unhandled case 'IEGroup'"
printImport _ (GHC.IEDoc _ _) =
    error "Language.Haskell.Stylish.Printer.Imports.printImportExport: unhandled case 'IEDoc'"
printImport _ (GHC.IEDocNamed _ _) =
    error "Language.Haskell.Stylish.Printer.Imports.printImportExport: unhandled case 'IEDocNamed'"


--------------------------------------------------------------------------------
printIeWrappedName :: GHC.LIEWrappedName GHC.RdrName -> P ()
printIeWrappedName lie = case GHC.unLoc lie of
    GHC.IEName      n -> putRdrName n
    GHC.IEPattern _ n -> putText "pattern" >> space >> putRdrName n
    GHC.IEType    _ n -> putText "type" >> space >> putRdrName n


mergeImports
    :: NonEmpty (GHC.LImportDecl GHC.GhcPs)
    -> NonEmpty (GHC.LImportDecl GHC.GhcPs)
mergeImports (x :| []) = x :| []
mergeImports (h :| (t : ts))
  | canMergeImport (GHC.unLoc h) (GHC.unLoc t) = mergeImports (mergeModuleImport h t :| ts)
  | otherwise = h :| mergeImportsTail (t : ts)
  where
    mergeImportsTail (x : y : ys)
      | canMergeImport (GHC.unLoc x) (GHC.unLoc y) = mergeImportsTail ((mergeModuleImport x y) : ys)
      | otherwise = x : mergeImportsTail (y : ys)
    mergeImportsTail xs = xs


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

importStats :: GHC.ImportDecl GHC.GhcPs -> ImportStats
importStats i =
    ImportStats (importModuleNameLength i) (isSource i) (isQualified i) (GHC.ideclSafe  i)

-- Computes length till module name, includes package name.
-- TODO: this should reuse code with the printer
importModuleNameLength :: GHC.ImportDecl GHC.GhcPs -> Int
importModuleNameLength imp =
    (case GHC.ideclPkgQual imp of
        Nothing -> 0
        Just sl -> 1 + length (stringLiteral sl)) +
    (length $ importModuleName imp)


--------------------------------------------------------------------------------
stringLiteral :: GHC.StringLiteral -> String
stringLiteral sl = case GHC.sl_st sl of
    GHC.NoSourceText -> show . GHC.unpackFS $ GHC.sl_fs sl
    GHC.SourceText s -> s


--------------------------------------------------------------------------------
isQualified :: GHC.ImportDecl GHC.GhcPs -> Bool
isQualified = (/=) GHC.NotQualified . GHC.ideclQualified

isHiding :: GHC.ImportDecl GHC.GhcPs -> Bool
isHiding = maybe False fst . GHC.ideclHiding

isSource :: GHC.ImportDecl GHC.GhcPs -> Bool
isSource = (==) GHC.IsBoot . GHC.ideclSource


--------------------------------------------------------------------------------
-- | Cleans up an import item list.
--
-- * Sorts import items.
-- * Sort inner import lists, e.g. `import Control.Monad (Monad (return, join))`
-- * Removes duplicates from import lists.
prepareImportList :: [GHC.LIE GHC.GhcPs] -> [GHC.LIE GHC.GhcPs]
prepareImportList =
  sortBy compareLIE . map (fmap prepareInner) .
  concatMap (toList . snd) . Map.toAscList . mergeByName
 where
  mergeByName
      :: [GHC.LIE GHC.GhcPs]
      -> Map.Map GHC.RdrName (NonEmpty (GHC.LIE GHC.GhcPs))
  mergeByName imports0 = Map.fromListWith
    -- Note that ideally every NonEmpty will just have a single entry and we
    -- will be able to merge everything into that entry.  Exotic imports can
    -- mess this up, though.  So they end up in the tail of the list.
    (\(x :| xs) (y :| ys) -> case ieMerge (GHC.unLoc x) (GHC.unLoc y) of
      Just z  -> (x $> z) :| (xs ++ ys)  -- Keep source from `x`
      Nothing -> x :| (xs ++ y : ys))
    [(GHC.ieName $ GHC.unLoc imp, imp :| []) | imp <- imports0]

  prepareInner :: GHC.IE GHC.GhcPs -> GHC.IE GHC.GhcPs
  prepareInner = \case
    -- Simplify `A ()` to `A`.
    GHC.IEThingWith x n GHC.NoIEWildcard [] -> GHC.IEThingAbs x n
    GHC.IEThingWith x n w ns ->
      GHC.IEThingWith x n w (sortBy (compareWrappedName `on` GHC.unLoc) ns)
    ie -> ie

  -- Merge two import items, assuming they have the same name.
  ieMerge :: GHC.IE GHC.GhcPs -> GHC.IE GHC.GhcPs -> Maybe (GHC.IE GHC.GhcPs)
  ieMerge l@(GHC.IEVar _ _)      _                  = Just l
  ieMerge _                  r@(GHC.IEVar _ _)      = Just r
  ieMerge (GHC.IEThingAbs _ _)   r                  = Just r
  ieMerge l                  (GHC.IEThingAbs _ _)   = Just l
  ieMerge l@(GHC.IEThingAll _ _) _                  = Just l
  ieMerge _                  r@(GHC.IEThingAll _ _) = Just r
  ieMerge (GHC.IEThingWith x0 n0 w0 ns0) (GHC.IEThingWith _ _ w1 ns1)
    | w0 /= w1  = Nothing
    | otherwise = Just $
        -- TODO: sort the `ns0 ++ ns1`?
        GHC.IEThingWith x0 n0 w0 (nubOn GHC.lieWrappedName $ ns0 ++ ns1)
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
