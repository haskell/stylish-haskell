{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
module Language.Haskell.Stylish.Printer.ModuleHeader
  ( printModuleHeader
  ) where


--------------------------------------------------------------------------------
import           Control.Monad                   (forM_, join, when)
import           Data.Foldable                   (toList)
import           Data.List                       (sortBy)
import           Data.Maybe                      (isJust)
import qualified GHC.Hs.Doc                      as GHC
import           GHC.Hs.Extension                (GhcPs)
import qualified GHC.Hs.Extension                as GHC
import           GHC.Hs.ImpExp                   (IE(..))
import qualified GHC.Hs.ImpExp                   as GHC
import qualified Module                          as GHC
import           SrcLoc                          (Located, GenLocated(..), SrcSpan(..))
import           SrcLoc                          (srcSpanStartLine, srcSpanEndLine)
import           Util                            (notNull)

--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Module
import           Language.Haskell.Stylish.Config (Config'(..))
import           Language.Haskell.Stylish.Printer
import           Language.Haskell.Stylish.Editor
import           Language.Haskell.Stylish.Block

printModuleHeader :: Config' -> Lines -> Module -> Lines
printModuleHeader cfg ls m =
  let
    header = moduleHeader m
    name = rawModuleName header
    haddocks = rawModuleHaddocks header
    exports = rawModuleExports header

    printedModuleHeader = runPrinter cfg (printHeader name exports haddocks)

    unsafeGetStart = \case
      (L (RealSrcSpan s) _) -> srcSpanStartLine s
      _ -> error "could not get start line of block"

    unsafeGetEnd = \case
      (L (RealSrcSpan s) _) -> srcSpanEndLine s
      _ -> error "could not get end line of block"

    getBlock loc =
      Block <$> fmap unsafeGetStart loc <*> fmap unsafeGetEnd loc

    adjustOffsetFrom :: Block a -> Block a -> Maybe (Block a)
    adjustOffsetFrom (Block s0 _) b2@(Block s1 e1)
      | s0 >= s1 && s0 >= e1 = Nothing
      | s0 >= s1 = Just (Block (s0 + 1) e1)
      | otherwise = Just b2

    nameBlock =
      getBlock name

    exportsBlock =
      join $ adjustOffsetFrom <$> nameBlock <*> getBlock exports

    mergeAdjacent (a : b : rest) | a `adjacent` b = merge a b : mergeAdjacent rest
    mergeAdjacent (a : rest) = a : mergeAdjacent rest
    mergeAdjacent [] = []

    deletes =
      fmap delete $ mergeAdjacent $ toList nameBlock <> toList exportsBlock

    startLine =
      maybe 1 blockStart nameBlock

    additions = [insert startLine printedModuleHeader]

    changes = deletes <> additions
  in
    applyChanges changes ls

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

  sep (newline >> space >> space >> comma >> space) (fmap printExports (sortBy compareOutputable exports))

  newline >> indent 2 (putText ")" >> space >> putText "where")
  where
    putOutputable = putText . showOutputable

    printExports :: GHC.LIE GhcPs -> P ()
    printExports (L _ export) = case export of
      IEVar _ name -> putOutputable name
      IEThingAbs _ name -> putOutputable name
      IEThingAll _ _name -> do
        undefined
        --printIeWrappedName name
        --space
        --putText "(..)"
      IEModuleContents _ (L _ _m) ->
        undefined
        --putText (moduleNameString m)
      IEThingWith _ _name _wildcard _imps _ ->
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
