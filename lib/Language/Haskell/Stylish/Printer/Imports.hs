{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
module Language.Haskell.Stylish.Printer.Imports
  ( printImports
  ) where

--------------------------------------------------------------------------------
import           Control.Monad                   (forM_, when)
import           Data.Function                   ((&))
import           Data.List                       (sortBy)
import           GHC.Hs.Extension                (GhcPs)
import qualified GHC.Hs.Extension                as GHC
import           GHC.Hs.ImpExp
import           Module                          (ModuleName, moduleNameString)
import           RdrName
import qualified SrcLoc                          as GHC

--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Config (Config'(..), ImportsPrinter(..))
import           Language.Haskell.Stylish.Module
import           Language.Haskell.Stylish.Printer

--------------------------------------------------------------------------------
printImports :: Config' -> Lines -> Module -> Lines
printImports cfg@(Config' {configImportsPrinter = printer}) ls m =
  if True then ls else
  runPrinter cfg importPrinter
  where
    imports = moduleImports m
    importList = rawImports imports

    importPrinter = case printer of
      DeclMinimizeDiffsPostQualified ->
        forM_ (sortImportDecls importList) \imp -> printPostQualified imp >> newline

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

  forM_ (ideclAs decl') \(GHC.L _ name) ->
    space >> putText "as" >> space >> putText (moduleNameString name)

  when (isHiding decl') (space >> putText "hiding" >> space)

  forM_ (snd <$> ideclHiding decl') \(GHC.L _ imports) ->
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
  IEModuleContents _ (GHC.L _ m) ->
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
  IEName n -> printRdrName n
  IEPattern n -> putText "pattern" >> space >> printRdrName n
  IEType n -> putText "type" >> space >> printRdrName n

printRdrName :: GHC.Located RdrName -> P ()
printRdrName (GHC.L _ n) = case n of
  Unqual name ->
    putText (showOutputable name)
  Qual modulePrefix name ->
    printModulePrefix modulePrefix >> dot >> putText (showOutputable name)
  Orig _ name ->
    putText (showOutputable name)
  Exact name ->
    putText (showOutputable name)

printModulePrefix :: ModuleName -> P ()
printModulePrefix = putText . moduleNameString

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

unLocated :: GHC.Located a -> a
unLocated (GHC.L _ a) = a

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

sortImportDecls :: [LImportDecl GhcPs] -> [LImportDecl GhcPs]
sortImportDecls = sortBy $ currycated \(a0, a1) ->
  compareOutputable (ideclName a0) (ideclName a1) <>
  compareOutputable a0 a1

currycated :: ((a, b) -> c) -> (GHC.Located a -> GHC.Located b -> c)
currycated f = \(GHC.L _ a) (GHC.L _ b) -> f (a, b)
