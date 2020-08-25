{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase #-}
module Language.Haskell.Stylish.Step.ImportsGHC
  ( Options (..)
  , step
  ) where

--------------------------------------------------------------------------------
import           Control.Monad                   (forM_, when)
import           Data.Function                   ((&))
import           Data.Foldable                   (toList)
import           Data.Maybe                      (listToMaybe)
import           Data.List                       (sortBy)
import           Data.List.NonEmpty              (NonEmpty(..))
import qualified Data.List.NonEmpty              as NonEmpty

--------------------------------------------------------------------------------
import           GHC.Hs.Extension                (GhcPs)
import qualified GHC.Hs.Extension                as GHC
import           GHC.Hs.ImpExp
import           Module                          (moduleNameString)
import           RdrName                         (RdrName)
import           Util                            (lastMaybe)
import           SrcLoc                          (Located, GenLocated(..))

--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Block
import           Language.Haskell.Stylish.Module
import           Language.Haskell.Stylish.Printer
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Editor
import           Language.Haskell.Stylish.GHC
import           Language.Haskell.Stylish.Step.Imports (Options(..))

step :: Maybe Int -> Options -> Step
step columns = makeStep "Imports (ghc-lib-parser)" . printImports columns

--------------------------------------------------------------------------------
printImports :: Maybe Int -> Options -> Lines -> Module -> Lines
printImports maxCols _ ls m = applyChanges changes ls
  where
    changes = concatMap (formatGroup maxCols m) (moduleImportGroups m)

formatGroup :: Maybe Int -> Module -> [Located Import] -> [Change String]
formatGroup _maxCols _m _imports@[] = []
formatGroup maxCols m imports@(impHead : impTail) = do
  let
    newLines = formatImports maxCols (impHead :| impTail) m

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

formatImports :: Maybe Int -> NonEmpty (Located Import) -> Module -> Lines
formatImports maxCols rawGroup m = runPrinter_ PrinterConfig [] m do
  let
    group
      = NonEmpty.sortWith unLocated rawGroup
      & mergeImports

  forM_ group \imp -> printPostQualified maxCols imp >> newline

--------------------------------------------------------------------------------
printPostQualified :: Maybe Int -> Located Import -> P ()
printPostQualified maxCols (L _ decl) = do
  let
    decl' = rawImport decl

  putText "import" >> space

  when (isSource decl) (putText "{-# SOURCE #-}" >> space)

  when (isSafe decl) (putText "safe" >> space)

  putText (moduleName decl)

  when (isQualified decl) (space >> putText "qualified")

  forM_ (ideclAs decl') \(L _ name) ->
    space >> putText "as" >> space >> putText (moduleNameString name)

  when (isHiding decl) (space >> putText "hiding")

  -- Since we might need to output the import module name several times, we
  -- need to save it to a variable:
  importDecl <- fmap putText getCurrentLine

  forM_ (snd <$> ideclHiding decl') \(L _ imports) ->
    let
      printedImports =
        fmap (printImport . unLocated) (sortImportList imports)

      impHead =
        listToMaybe printedImports

      impTail =
        drop 1 printedImports
    in do
      space
      putText "("

      forM_ impHead id

      forM_ impTail \printedImport -> do
        len <- getCurrentLineLength
        if canSplit len then do
          putText ")"
          newline
          importDecl
          space
          putText "("
        else do
          comma
          space

        printedImport

      putText ")"
  where
    canSplit len = and
      [ -- If the max cols have been surpassed, split:
        maybe False (len >=) maxCols
        -- Splitting a 'hiding' import changes the scope, don't split hiding:
      , not (isHiding decl)
      ]

--------------------------------------------------------------------------------
printImport :: IE GhcPs -> P ()
printImport = \case
  IEVar _ name ->
    printIeWrappedName name
  IEThingAbs _ name ->
    printIeWrappedName name
  IEThingAll _ name -> do
    printIeWrappedName name
    space
    putText "(..)"
  IEModuleContents _ (L _ m) ->
    putText (moduleNameString m)
  IEThingWith _ name _wildcard imps _ -> do
    printIeWrappedName name
    space
    parenthesize $
      sep (comma >> space) (printIeWrappedName <$> sortBy compareOutputable imps)
  IEGroup _ _ _ ->
    error "Language.Haskell.Stylish.Printer.Imports.printImportExport: unhandled case 'IEGroup'"
  IEDoc _ _ ->
    error "Language.Haskell.Stylish.Printer.Imports.printImportExport: unhandled case 'IEDoc'"
  IEDocNamed _ _ ->
    error "Language.Haskell.Stylish.Printer.Imports.printImportExport: unhandled case 'IEDocNamed'"
  XIE ext ->
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
sortImportList = sortBy $ currycated \case
  (IEVar _ n0, IEVar _ n1) -> compareOutputable n0 n1

  (IEThingAbs _ n0, IEThingAbs _ n1) -> compareOutputable n0 n1
  (IEThingAbs _ n0, IEThingAll _ n1) -> compareOutputable n0 n1
  (IEThingAbs _ n0, IEThingWith _ n1 _ _ _) -> compareOutputable n0 n1 <> LT

  (IEThingAll _ n0, IEThingAll _ n1) -> compareOutputable n0 n1
  (IEThingAll _ n0, IEThingAbs _ n1) -> compareOutputable n0 n1
  (IEThingAll _ n0, IEThingWith _ n1 _ _ _) -> compareOutputable n0 n1 <> LT

  (IEThingWith _ n0 _ _ _, IEThingWith _ n1 _ _ _) -> compareOutputable n0 n1
  (IEThingWith _ n0 _ _ _, IEThingAll _ n1) -> compareOutputable n0 n1 <> GT
  (IEThingWith _ n0 _ _ _, IEThingAbs _ n1) -> compareOutputable n0 n1 <> GT

  (IEVar _ _, _) -> GT
  (_, IEVar _ _) -> LT
  (IEThingAbs _ _, _) -> GT
  (_, IEThingAbs _ _) -> LT
  (IEThingAll _ _, _) -> GT
  (_, IEThingAll _ _) -> LT
  (IEThingWith _ _ _ _ _, _) -> GT
  (_, IEThingWith _ _ _ _ _) -> LT

  _ -> EQ

currycated :: ((a, b) -> c) -> (Located a -> Located b -> c)
currycated f = \(L _ a) (L _ b) -> f (a, b)
