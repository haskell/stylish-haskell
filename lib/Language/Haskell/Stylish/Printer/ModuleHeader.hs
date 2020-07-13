{-# LANGUAGE BlockArguments #-}
module Language.Haskell.Stylish.Printer.ModuleHeader
  ( printModuleHeader
  ) where


--------------------------------------------------------------------------------
import           Control.Monad                   (forM_, when)
import           Data.Maybe                      (isJust)
import qualified GHC.Hs.Doc                      as GHC
import           GHC.Hs.Extension                (GhcPs)
import qualified GHC.Hs.Extension                as GHC
import           GHC.Hs.ImpExp                   (IE(..))
import qualified GHC.Hs.ImpExp                   as GHC
import qualified Module                          as GHC
import           SrcLoc                          (Located, GenLocated(..))
import           Util                            (notNull)

--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Module
import           Language.Haskell.Stylish.Config (Config'(..))
import           Language.Haskell.Stylish.Printer

printModuleHeader :: Config' -> Comments -> ModuleHeader -> Lines
printModuleHeader cfg _ header =
  let
    name = rawModuleName header
    haddocks = rawModuleHaddocks header
    exports = rawModuleExports header
  in
    runPrinter cfg (printHeader name exports haddocks)

printHeader ::
     Maybe (Located GHC.ModuleName)
  -> Maybe (Located [GHC.LIE GhcPs])
  -> Maybe GHC.LHsDocString
  -> P ()
printHeader mname mexps _ = do
  forM_ mname \(L _ name) -> do
    putText "module"
    space
    putText (showOutputable name)

  maybe
    (when (isJust mname) do newline >> space >> space >> putText "where")
    printExportList
    mexps

printExportList :: Located [GHC.LIE GhcPs] -> P ()
printExportList (L _ exports) = do
  newline
  indent 2 (putText "(") >> when (notNull exports) space

  sep (newline >> space >> space >> comma >> space) (fmap printExports exports)

  newline >> indent 2 (putText ")" >> space >> putText "where")
  where
    putOutputable = putText . showOutputable

    printExports :: GHC.LIE GhcPs -> P ()
    printExports (L _ export) = case export of
      IEVar _ name -> putOutputable name
      IEThingAbs _ name -> putOutputable name
      IEThingAll _ name -> do
        undefined
        --printIeWrappedName name
        --space
        --putText "(..)"
      IEModuleContents _ (L _ m) ->
        undefined
        --putText (moduleNameString m)
      IEThingWith _ name _wildcard imps _ ->
        undefined
        --let
        --  sortedImps = flip sortBy imps \(GHC.L _ a0) (GHC.L _ a1) -> compareOutputable a0 a1
        --in do
        --  printIeWrappedName name
        --  space
        --  parenthesize $ sep (comma >> space) (fmap printIeWrappedName sortedImps)
      IEGroup _ _ _ ->
        error "Language.Haskell.Stylish.Printer.Imports.printImportExport: unhandled case 'IEGroup'"
      IEDoc _ _ ->
        error "Language.Haskell.Stylish.Printer.Imports.printImportExport: unhandled case 'IEDoc'"
      IEDocNamed _ _ ->
        error "Language.Haskell.Stylish.Printer.Imports.printImportExport: unhandled case 'IEDocNamed'"
      XIE ext ->
        GHC.noExtCon ext
