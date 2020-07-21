{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase #-}
module Language.Haskell.Stylish.Step.Imports'
  ( Config (..)
  , step
  ) where

--------------------------------------------------------------------------------
import           Control.Monad                   (forM_, when)
import           Data.Function                   ((&))
import           Data.Foldable                   (toList)
import           Data.Maybe                      (listToMaybe)
import           Data.List                       (sortBy)
import           Data.List.NonEmpty              (NonEmpty)
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

data Config = Config

step :: Config -> Step
step = makeStep "Imports" . printImports

--------------------------------------------------------------------------------
printImports :: Config -> Lines -> Module -> Lines
printImports _ ls m = formatForImportGroups ls m (moduleImportGroups m)

formatForImportGroups :: Lines -> Module -> [Imports] -> Lines
formatForImportGroups ls _m [] = ls
formatForImportGroups ls m (group : rest) = formatForImportGroups formattedGroup m rest
  where
    formattedGroup :: Lines
    formattedGroup =
      let
        imports =
          rawImports group

        relevantComments =
          []

        importsBlock = Block
          <$> importStart
          <*> importEnd

        importStart
          = listToMaybe imports
          & fmap getStartLineUnsafe

        importEnd
          = lastMaybe imports
          & fmap getEndLineUnsafe

        formatting = runPrinter_ PrinterConfig relevantComments m do
          importsWithComments <- sortedAttachedComments imports
          forM_ importsWithComments \(_, importGroup) -> do
            forM_ (sortImportDecls importGroup) \imp -> printPostQualified imp >> newline
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
printPostQualified :: LImportDecl GhcPs -> P ()
printPostQualified decl = do
  let
    decl' = unLocated decl

  putText "import" >> space

  when (ideclSource decl') (putText "{-# SOURCE #-}" >> space)

  when (ideclSafe decl') (putText "safe" >> space)

  putText (moduleName decl)

  when (isQualified decl) (space >> putText "qualified")

  forM_ (ideclAs decl') \(L _ name) ->
    space >> putText "as" >> space >> putText (moduleNameString name)

  when (isHiding decl') (space >> putText "hiding")

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

moduleName :: LImportDecl GhcPs -> String
moduleName
  = moduleNameString
  . unLocated
  . ideclName
  . unLocated

isQualified :: LImportDecl GhcPs -> Bool
isQualified
  = (/=) NotQualified
  . ideclQualified
  . unLocated

isHiding :: ImportDecl GhcPs -> Bool
isHiding
  = maybe False fst
  . ideclHiding

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

sortImportDecls :: NonEmpty (LImportDecl GhcPs) -> NonEmpty (LImportDecl GhcPs)
sortImportDecls = NonEmpty.sortBy $ currycated \(a0, a1) ->
  compareOutputable (ideclName a0) (ideclName a1) <>
  compareOutputable a0 a1

currycated :: ((a, b) -> c) -> (Located a -> Located b -> c)
currycated f = \(L _ a) (L _ b) -> f (a, b)
