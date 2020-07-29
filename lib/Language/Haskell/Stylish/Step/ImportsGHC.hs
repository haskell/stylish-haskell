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

step :: Options -> Step
step = makeStep "Imports (ghc-lib-parser)" . printImports

--------------------------------------------------------------------------------
printImports :: Options -> Lines -> Module -> Lines
printImports _ ls m = formatForImportGroups ls m (moduleImportGroups m)

formatForImportGroups :: Lines -> Module -> [[Located Import]] -> Lines
formatForImportGroups ls _m [] = ls
formatForImportGroups ls m (group : rest) = formatForImportGroups formattedGroup m rest
  where
    formattedGroup :: Lines
    formattedGroup =
      let
        relevantComments =
          []

        importsBlock = Block
          <$> importStart
          <*> importEnd

        importStart
          = listToMaybe group
          & fmap getStartLineUnsafe

        importEnd
          = lastMaybe group
          & fmap getEndLineUnsafe

        formatting = runPrinter_ PrinterConfig relevantComments m do
          importsWithComments <- sortedAttachedComments group
          forM_ importsWithComments \(_, rawGroup) -> do
            let
              importGroup
                = NonEmpty.sortWith unLocated rawGroup
                & mergeImports
            forM_ importGroup \imp -> printPostQualified imp >> newline
      in
        case importStart of
          Just start ->
            let
              deletes = fmap delete $ toList importsBlock
              additions = [insert start formatting]
            in
              applyChanges (deletes <> additions) ls
          Nothing -> ls

--------------------------------------------------------------------------------
printPostQualified :: Located Import -> P ()
printPostQualified (L _ decl) = do
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

  forM_ (snd <$> ideclHiding decl') \(L _ imports) ->
    let
      printedImports =
        fmap (printImport . unLocated) (sortImportList imports)

      separated =
        sep (comma >> space)
    in
      space >> parenthesize (separated printedImports)

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
