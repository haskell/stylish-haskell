{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Language.Haskell.Stylish.Step.ImportsGHC
  ( Options (..)
  , step
  ) where

--------------------------------------------------------------------------------
import           Control.Monad                   (forM_, when)
import           Data.Function                   ((&))
import           Data.Foldable                   (toList)
import           Data.Ord                        (comparing)
import           Data.Maybe                      (isJust, listToMaybe)
import           Data.List                       (sortBy, isPrefixOf)
import           Data.List.NonEmpty              (NonEmpty(..))
import qualified Data.List.NonEmpty              as NonEmpty

--------------------------------------------------------------------------------
import           GHC.Hs.Extension                (GhcPs)
import qualified GHC.Hs.Extension                as GHC
import           GHC.Hs.ImpExp
import           Module                          (moduleNameString)
import           RdrName                         (RdrName)
import           Util                            (lastMaybe)
import           SrcLoc                          (Located, GenLocated(..), unLoc)


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Block
import           Language.Haskell.Stylish.Module
import           Language.Haskell.Stylish.Printer
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Editor
import           Language.Haskell.Stylish.GHC
import           Language.Haskell.Stylish.Step.Imports hiding (step)


step :: Maybe Int -> Options -> Step
step columns = makeStep "Imports (ghc-lib-parser)" . printImports columns

--------------------------------------------------------------------------------
printImports :: Maybe Int -> Options -> Lines -> Module -> Lines
printImports maxCols align ls m = applyChanges changes ls
  where
    groups = moduleImportGroups m
    changes = concatMap (formatGroup maxCols align m) groups

formatGroup :: Maybe Int -> Options -> Module -> [Located Import] -> [Change String]
formatGroup _maxCols _align _m _imports@[] = []
formatGroup maxCols align m imports@(impHead : impTail) = do
  let
    newLines = formatImports maxCols align (impHead :| impTail) m

  toList $ fmap (\block -> change block (const newLines)) (importBlock imports)

importBlock :: [Located a] -> Maybe (Block String)
importBlock group = Block <$> importStart <*> importEnd
  where
    importStart
      = listToMaybe group
      & fmap getStartLineUnsafe

    importEnd
      = lastMaybe group
      & fmap getEndLineUnsafe

formatImports :: Maybe Int -> Options -> NonEmpty (Located Import) -> Module -> Lines
formatImports maxCols align rawGroup m = runPrinter_ PrinterConfig [] m do
  let 
     
    group
      = NonEmpty.sortWith unLocated rawGroup
      & mergeImports

    unLocatedGroup = fmap unLocated $ toList group

    anyQual = any isQualified unLocatedGroup

    fileAlign = case importAlign align of
      File -> anyQual
      _    -> False
 
    align' = importAlign align
    padModuleNames' = padModuleNames align
    padNames = align' /= None && padModuleNames'
    padQual  = case align' of
      Global -> True
      File   -> fileAlign
      Group  -> anyQual
      None   -> False
        
    longest = longestImport unLocatedGroup

  forM_ group \imp -> printQualified maxCols align padQual padNames longest imp >> newline

--------------------------------------------------------------------------------
printQualified :: Maybe Int -> Options -> Bool -> Bool -> Int -> Located Import -> P ()
printQualified maxCols Options{..} padQual padNames longest (L _ decl) = do
  let
    decl'         = rawImport decl
    _listPadding' = listPaddingValue (6 + 1 + qualifiedLength) listPadding

  putText "import" >> space

  when (isSource decl) (putText "{-# SOURCE #-}" >> space)

  when (isSafe decl) (putText "safe" >> space)

  when (isQualified decl) (putText "qualified" >> space)

  padQualified decl padQual

  putText (moduleName decl)

  -- Only print spaces if something follows.
  when (isJust (ideclAs decl') || isHiding decl ||
          not (null $ ideclHiding decl')) $
      padImportsList decl padNames longest

  forM_ (ideclAs decl') \(L _ name) ->
    space >> putText "as" >> space >> putText (moduleNameString name)

  when (isHiding decl) (space >> putText "hiding") 

  -- Since we might need to output the import module name several times, we
  -- need to save it to a variable:
  importDecl <- fmap putText getCurrentLine

  forM_ (snd <$> ideclHiding decl') \(L _ imports) ->
    let
      printedImports = -- [P ()]
        fmap ((printImport Options{..}) . unLocated) (sortImportList imports)

      impHead =
        listToMaybe printedImports

      impTail =
        drop 1 printedImports
    in do
      space
      putText "("
      
      when spaceSurround space

      forM_ impHead id

      forM_ impTail \printedImport -> do
        len <- getCurrentLineLength
        if canSplit (len) then do
          putText ")"
          newline
          importDecl
          space
          putText "("
        else do
          comma
          space

        printedImport
       
      when spaceSurround space
      putText ")"
  where
    canSplit len = and
      [ -- If the max cols have been surpassed, split:
        maybe False (len >=) maxCols
        -- Splitting a 'hiding' import changes the scope, don't split hiding:
      , not (isHiding decl)
      ]

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
longestImport :: [Import] -> Int
longestImport = maximum . map importLength

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

