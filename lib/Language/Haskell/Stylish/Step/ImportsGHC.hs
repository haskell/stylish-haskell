{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Language.Haskell.Stylish.Step.ImportsGHC
  ( Options (..)
  , step
  ) where

--------------------------------------------------------------------------------
import           Control.Monad                   (forM_, when, void)
import           Data.Char                       (isUpper)
import           Data.Function                   ((&))
import           Data.Functor                    (($>))
import           Data.Foldable                   (toList)
import           Data.Ord                        (comparing)
import           Data.Maybe                      (isJust)
import           Data.List                       (sortBy, sortOn)
import           Data.List.NonEmpty              (NonEmpty(..))
import qualified Data.List.NonEmpty              as NonEmpty
import qualified Data.Map                        as Map
import qualified Data.Set                        as Set


--------------------------------------------------------------------------------
import           GHC.Hs.Extension                (GhcPs)
import qualified GHC.Hs.Extension                as GHC
import           GHC.Hs.ImpExp
import           Module                          (moduleNameString)
import           RdrName                         (RdrName)
import           SrcLoc                          (Located, GenLocated(..), unLoc)


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Block
import           Language.Haskell.Stylish.Module
import           Language.Haskell.Stylish.Printer
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Editor
import           Language.Haskell.Stylish.GHC
import           Language.Haskell.Stylish.Step.Imports hiding (step)
import           Language.Haskell.Stylish.Util


step :: Maybe Int -> Options -> Step
step columns = makeStep "Imports (ghc-lib-parser)" . printImports columns

--------------------------------------------------------------------------------
printImports :: Maybe Int -> Options -> Lines -> Module -> Lines
printImports maxCols align ls m = applyChanges changes ls
  where
    groups = moduleImportGroups m
    moduleLongestImport = longestImport . fmap unLoc $ concatMap toList groups
    moduleAnyQual = any isQualified . fmap unLoc $ concatMap toList groups
    changes = do
        group <- groups
        pure $ formatGroup maxCols align m
            moduleLongestImport moduleAnyQual group

formatGroup
    :: Maybe Int -> Options -> Module -> Int -> Bool
    -> NonEmpty (Located Import) -> Change String
formatGroup maxCols options m moduleLongestImport moduleAnyQual imports =
    let newLines = formatImports maxCols options m
            moduleLongestImport moduleAnyQual imports in
    change (importBlock imports) (const newLines)

importBlock :: NonEmpty (Located a) -> Block String
importBlock group = Block
    (getStartLineUnsafe $ NonEmpty.head group)
    (getEndLineUnsafe   $ NonEmpty.last group)

formatImports
    :: Maybe Int  -- ^ Max columns.
    -> Options    -- ^ Options.
    -> Module     -- ^ Module.
    -> Int        -- ^ Longest import in module.
    -> Bool       -- ^ Qualified import is present in module.
    -> NonEmpty (Located Import) -> Lines
formatImports maxCols options m moduleLongestImport moduleAnyQual rawGroup =
  runPrinter_ (PrinterConfig maxCols) [] m do
  let 
     
    group
      = NonEmpty.sortWith unLocated rawGroup
      & mergeImports

    unLocatedGroup = fmap unLocated $ toList group

    anyQual = any isQualified unLocatedGroup

    align' = importAlign options
    padModuleNames' = padModuleNames options
    padNames = align' /= None && padModuleNames'
    padQual  = case align' of
      Global -> True
      File   -> moduleAnyQual
      Group  -> anyQual
      None   -> False

    longest = case align' of
        Global -> moduleLongestImport
        File   -> moduleLongestImport
        Group  -> longestImport unLocatedGroup
        None   -> 0

  forM_ group \imp -> printQualified options padQual padNames longest imp >> newline

--------------------------------------------------------------------------------
printQualified :: Options -> Bool -> Bool -> Int -> Located Import -> P ()
printQualified Options{..} padQual padNames longest (L _ decl) = do
  let
    decl'         = rawImport decl
    _listPadding' = listPaddingValue (6 + 1 + qualifiedLength) listPadding

  putText "import" >> space

  when (isSource decl) (putText "{-# SOURCE #-}" >> space)

  when (isSafe decl) (putText "safe" >> space)

  when (isQualified decl) (putText "qualified" >> space)

  padQualified decl padQual

  moduleNamePosition <- length <$> getCurrentLine
  putText (moduleName decl)

  -- Only print spaces if something follows.
  when (isJust (ideclAs decl') || isHiding decl ||
          not (null $ ideclHiding decl')) $
      padImportsList decl padNames longest

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
            fmap ((printImport Options{..}) . unLocated)
            (prepareImportList imports)

      -- Since we might need to output the import module name several times, we
      -- need to save it to a variable:
      wrapPrefix <- case listAlign of
        AfterAlias -> pure $ replicate (afterAliasPosition + 2) ' '
        WithAlias -> pure $ replicate (beforeAliasPosition + 1) ' '
        Repeat -> fmap (++ " (") getCurrentLine
        WithModuleName -> pure $ replicate (moduleNamePosition + offset) ' '
        NewLine -> pure $ replicate offset ' '

      let -- Try to put everything on one line.
          printAsSingleLine = forM_ printedImports $ \(imp, start, end) -> do
            when start $ putText "("
            imp
            if end then putText ")" else comma >> space

          -- Try to put everything one by one, wrapping if that fails.
          printAsInlineWrapping wprefix = forM_ printedImports $
            \(imp, start, end) ->
            patchForRepeatHiding $ wrapping
              (do
                if start then putText "(" else space
                imp
                if end then putText ")" else comma)
              (do
                case listAlign of
                    -- In 'Repeat' mode, end lines with ')' rather than ','.
                    Repeat | not start -> modifyCurrentLine . withLast $
                        \c -> if c == ',' then ')' else c
                    _ -> pure ()
                newline
                void wprefix
                imp
                if end then putText ")" else comma)
          -- Put everything on a separate line.
          printAsMultiLine = forM_ printedImports $ \(imp, start, end) -> do
            when start $ modifyCurrentLine trimRight  -- We added some spaces.
            newline
            putOffset
            if start then putText "( " else putText ", "
            imp
            when end $ newline >> putOffset >> putText ")"

      when spaceSurround space
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

      -- when spaceSurround space
      -- putText ")"
  where
      {-
    canSplit len = and
      [ -- If the max cols have been surpassed, split:
        maybe False (len >=) maxCols
        -- Splitting a 'hiding' import changes the scope, don't split hiding:
      , not (isHiding decl)
      ]
      -}

    -- We cannot wrap/repeat 'hiding' imports since then we would get multiple
    -- imports hiding different things.
    patchForRepeatHiding = case listAlign of
        Repeat | isHiding decl -> withColumns Nothing
        _                      -> id

    qualifiedDecl | isQualified decl = ["qualified"]
                  | padQual          =
                    if isSource decl
                    then []
                    else if isSafe decl
                         then ["    "]
                         else ["         "]
                  | otherwise        = []
    qualifiedLength = if null qualifiedDecl then 0 else 1 + sum (map length qualifiedDecl)


--------------------------------------------------------------------------------
printImport :: Options -> IE GhcPs -> P ()
printImport Options{..} (IEVar _ name) = do
    printIeWrappedName name
printImport _ (IEThingAbs _ name) = do
    printIeWrappedName name
printImport _ (IEThingAll _ name) = do
    printIeWrappedName name
    space
    putText "(..)"
printImport _ (IEModuleContents _ (L _ m)) = do
    putText (moduleNameString m)
printImport Options{..} (IEThingWith _ name _wildcard imps _) = do
    printIeWrappedName name
    when separateLists space
    parenthesize $
      sep (comma >> space) (printIeWrappedName <$> imps)
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
longestImport :: (Foldable f, Functor f) => f Import -> Int
longestImport xs = if null xs then 0 else maximum $ fmap importLength xs

-- computes length till module name
importLength :: Import -> Int
importLength i =
  let
    srcLength  | isSource i = length "{# SOURCE #}"
               | otherwise  = 0
    qualLength = length "qualified"
    nameLength = length $ moduleName i
  in
    srcLength + qualLength + nameLength

--------------------------------------------------------------------------------
padQualified :: Import -> Bool -> P ()
padQualified i padQual = do
  let pads = length "qualified"
  if padQual && not (isQualified i)
  then (putText $ replicate pads ' ') >> space
  else pure ()

padImportsList :: Import -> Bool -> Int -> P ()
padImportsList i padNames longest = do
  let diff = longest - importLength i
  if padNames
  then putText $ replicate diff ' '
  else pure ()
                   
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
  sortBy compareImportLIE . map (fmap prepareInner) .
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

  -- | TODO: get rid off this by adding a properly sorting newtype around
  -- 'RdrName'.
  compareImportLIE :: LIE GhcPs -> LIE GhcPs -> Ordering
  compareImportLIE = comparing $ ieKey . unLoc

  prepareInner :: IE GhcPs -> IE GhcPs
  prepareInner = \case
    -- Simplify `A ()` to `A`.
    IEThingWith x n NoIEWildcard [] [] -> IEThingAbs x n
    IEThingWith x n w ns fs -> IEThingWith x n w (sortOn nameKey ns) fs
    ie -> ie

  -- | The implementation is a bit hacky to get proper sorting for input specs:
  -- constructors first, followed by functions, and then operators.
  ieKey :: IE GhcPs -> (Int, String)
  ieKey = \case
    IEVar _ n             -> nameKey n
    IEThingAbs _ n        -> nameKey n
    IEThingAll _ n        -> nameKey n
    IEThingWith _ n _ _ _ -> nameKey n
    _                     -> (2, "")

  nameKey n = case showOutputable n of
    o@('(' : _)             -> (2 :: Int, o)
    o@(o0 : _) | isUpper o0 -> (0, o)
    o                       -> (1, o)

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
        IEThingWith x0 n0 w0 (nubOn (unwrapName . unLocated) $ ns0 ++ ns1) []
  ieMerge _ _ = Nothing

  unwrapName :: IEWrappedName n -> n
  unwrapName (IEName n)    = unLocated n
  unwrapName (IEPattern n) = unLocated n
  unwrapName (IEType n)    = unLocated n


--------------------------------------------------------------------------------
listPaddingValue :: Int -> ListPadding -> Int
listPaddingValue _ (LPConstant n) = n
listPaddingValue n LPModuleName   = n


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
