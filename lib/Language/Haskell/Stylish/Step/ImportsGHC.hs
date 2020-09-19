{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Language.Haskell.Stylish.Step.ImportsGHC
  ( Options (..)
  , step
  ) where

--------------------------------------------------------------------------------
import           Control.Monad                   (forM_, when, unless)
import           Data.Function                   ((&))
import           Data.Foldable                   (toList)
import           Data.Ord                        (comparing)
import           Data.Maybe                      (isJust)
import           Data.List                       (sortBy, isPrefixOf)
import           Data.List.NonEmpty              (NonEmpty(..))
import qualified Data.List.NonEmpty              as NonEmpty


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

  case snd <$> ideclHiding decl' of
    Nothing            -> pure ()
    Just (L _ [])      -> putText " ()"
    Just (L _ imports) -> do
      let printedImports = -- [P ()]
            fmap ((printImport Options{..}) . unLocated) (sortImportList imports)
      putText " ("

      -- Since we might need to output the import module name several times, we
      -- need to save it to a variable:
      let offset = case listPadding of LPConstant n -> n; LPModuleName -> 0
      wrapPrefix <- case listAlign of
          AfterAlias -> pure $ replicate (afterAliasPosition + 2) ' '
          WithAlias -> pure $ replicate (beforeAliasPosition + 1) ' '
          Repeat -> getCurrentLine
          WithModuleName -> pure $ replicate (moduleNamePosition + offset) ' '
          NewLine -> pure $ replicate offset ' '

      when spaceSurround space
      forM_ (flagEnds printedImports) $ \(imp, isFirst, isLast) -> do
        wrapping
          (do
            unless isFirst space
            imp
            if isLast then putText ")" else comma)
          (do
            case listAlign of
                -- In 'Repeat' mode, end lines with ')' rather than ','.
                Repeat | not isFirst -> modifyCurrentLine . withLast $
                    \c -> if c == ',' then ')' else c
                _ -> pure ()
            newline
            putText wrapPrefix
            imp
            if isLast then putText ")" else comma)

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
      sep (comma >> space) (printIeWrappedName <$> sortBy compareOutputable imps)
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

sortImportList :: [LIE GhcPs] -> [LIE GhcPs]
sortImportList = sortBy compareImportLIE


--------------------------------------------------------------------------------
-- | The implementation is a bit hacky to get proper sorting for input specs:
-- constructors first, followed by functions, and then operators.
compareImportLIE :: LIE GhcPs -> LIE GhcPs -> Ordering
compareImportLIE = comparing $ key . unLoc
  where
    key :: IE GhcPs -> (Int, Bool, String)
    key (IEVar _ n)             = let o = showOutputable n in
                                  (1, "(" `isPrefixOf` o, o)
    key (IEThingAbs _ n)        = (0, False, showOutputable n)
    key (IEThingAll _ n)        = (0, False, showOutputable n)
    key (IEThingWith _ n _ _ _) = (0, False, showOutputable n)
    key _                       = (2, False, "")


--------------------------------------------------------------------------------
listPaddingValue :: Int -> ListPadding -> Int
listPaddingValue _ (LPConstant n) = n
listPaddingValue n LPModuleName   = n

