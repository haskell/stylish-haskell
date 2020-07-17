{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
module Language.Haskell.Stylish.Step.ModuleHeader
  ( Config (..)
  , step
  ) where

--------------------------------------------------------------------------------
import           ApiAnnotation                   (AnnotationComment(..), AnnKeywordId(..))
import           Control.Monad                   (forM_, join, when)
import           Data.Foldable                   (find, toList)
import           Data.Function                   ((&))
import           Data.List                       (sort, sortBy)
import           Data.List.NonEmpty              (NonEmpty(..))
import           Data.Maybe                      (listToMaybe, isJust)
import qualified GHC.Hs.Doc                      as GHC
import           GHC.Hs.Extension                (GhcPs)
import qualified GHC.Hs.Extension                as GHC
import           GHC.Hs.ImpExp                   (IE(..))
import qualified GHC.Hs.ImpExp                   as GHC
import qualified Module                          as GHC
import           SrcLoc                          (Located, GenLocated(..), SrcSpan(..))
import           SrcLoc                          (RealLocated)
import           SrcLoc                          (srcSpanStartLine, srcSpanEndLine)
import           Util                            (notNull)

--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Block
import           Language.Haskell.Stylish.Editor
import           Language.Haskell.Stylish.Module
import           Language.Haskell.Stylish.Printer
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Util (getStartLineUnsafe, getEndLineUnsafe)


data Config = Config

step :: Config -> Step
step = makeStep "Module header" . printModuleHeader

printModuleHeader :: Config -> Lines -> Module -> Lines
printModuleHeader _ ls m =
  let
    header = moduleHeader m
    name = rawModuleName header
    haddocks = rawModuleHaddocks header
    exports = rawModuleExports header
    annotations = rawModuleAnnotations m

    relevantComments :: [RealLocated AnnotationComment]
    relevantComments
      = moduleComments m
      & rawComments
      & dropAfter exports
      & dropBefore name

    dropAfter loc xs = case loc of
      Just (L (RealSrcSpan rloc) _) ->
        filter (\(L x _) -> srcSpanEndLine rloc >= srcSpanStartLine x) xs
      _ -> xs

    dropBefore loc xs = case loc of
      Just (L (RealSrcSpan rloc) _) ->
        filter (\(L x _) -> srcSpanStartLine rloc <= srcSpanEndLine x) xs
      _ -> xs

    printedModuleHeader =
      runPrinter PrinterConfig relevantComments (printHeader name exports haddocks)

    getBlock loc =
      Block <$> fmap getStartLineUnsafe loc <*> fmap getEndLineUnsafe loc

    adjustOffsetFrom :: Block a -> Block a -> Maybe (Block a)
    adjustOffsetFrom (Block s0 _) b2@(Block s1 e1)
      | s0 >= s1 && s0 >= e1 = Nothing
      | s0 >= s1 = Just (Block (s0 + 1) e1)
      | otherwise = Just b2

    nameBlock =
      getBlock name

    exportsBlock =
      join $ adjustOffsetFrom <$> nameBlock <*> getBlock exports

    whereM :: Maybe SrcSpan
    whereM
      = annotations
      & filter (\(((_, w), _)) -> w == AnnWhere)
      & fmap (head . snd) -- get position of annot
      & sort
      & listToMaybe

    isModuleHeaderWhere :: Block a -> Bool
    isModuleHeaderWhere w
      = not
      . overlapping
      $ [w] <> toList nameBlock <> toList exportsBlock

    toLineBlock :: SrcSpan -> Block a
    toLineBlock (RealSrcSpan s) = Block (srcSpanStartLine s) (srcSpanEndLine s)
    toLineBlock s
      = error
      $ "'where' block was not a RealSrcSpan" <> show s

    whereBlock
      = whereM
      & fmap toLineBlock
      & find isModuleHeaderWhere

    deletes =
      fmap delete $ mergeAdjacent $ toList nameBlock <> toList exportsBlock <> toList whereBlock

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
  forM_ mname \(L loc name) -> do
    putText "module"
    space
    putText (showOutputable name)
    attachEolComment loc

  maybe
    (when (isJust mname) do newline >> space >> space >> putText "where")
    printExportList
    mexps

attachEolComment :: SrcSpan -> P ()
attachEolComment = \case
  UnhelpfulSpan _ -> pure ()
  RealSrcSpan rspan ->
    removeLineComment (srcSpanStartLine rspan) >>= mapM_ \c -> space >> putComment c

attachEolCommentEnd :: SrcSpan -> P ()
attachEolCommentEnd = \case
  UnhelpfulSpan _ -> pure ()
  RealSrcSpan rspan ->
    removeLineComment (srcSpanEndLine rspan) >>= mapM_ \c -> space >> putComment c

printExportList :: Located [GHC.LIE GhcPs] -> P ()
printExportList (L srcLoc exports) = do
  newline
  indent 2 (putText "(") >> when (notNull exports) space

  exportsWithComments <- sortedAttachedComments exports

  printExports exportsWithComments

  putText ")" >> space >> putText "where" >> attachEolCommentEnd srcLoc
  where
    putOutputable = putText . showOutputable

    printExports :: [([AnnotationComment], NonEmpty (GHC.LIE GhcPs))] -> P ()
    printExports (([], firstInGroup :| groupRest) : rest) = do
      printExport firstInGroup
      newline
      spaces 2
      printExportsGroupTail groupRest
      printExportsTail rest
    printExports ((firstComment : comments, firstExport :| groupRest) : rest) = do
      putComment firstComment >> newline >> spaces 2
      forM_ comments \c -> spaces 2 >> putComment c >> newline >> spaces 2
      spaces 2
      printExport firstExport
      newline
      spaces 2
      printExportsGroupTail groupRest
      printExportsTail rest
    printExports [] =
      newline >> spaces 2

    printExportsTail :: [([AnnotationComment], NonEmpty (GHC.LIE GhcPs))] -> P ()
    printExportsTail = mapM_ \(comments, exported) -> do
      forM_ comments \c -> spaces 2 >> putComment c >> newline >> spaces 2
      forM_ exported \export -> do
        comma >> space >> printExport export
        newline >> spaces 2

    printExportsGroupTail :: [GHC.LIE GhcPs] -> P ()
    printExportsGroupTail (x : xs) = printExportsTail [([], x :| xs)]
    printExportsGroupTail [] = pure ()

    printExport :: GHC.LIE GhcPs -> P ()
    printExport (L _ export) = case export of
      IEVar _ name -> putOutputable name
      IEThingAbs _ name -> putOutputable name
      IEThingAll _ name -> do
        putOutputable name
        space
        putText "(..)"
      IEModuleContents _ (L _ m) -> do
        putText "module"
        space
        putText (showOutputable m)
      IEThingWith _ name _wildcard imps _ -> do
        putOutputable name
        space
        putText "("
        sep (comma >> space) (fmap putOutputable (sortBy compareOutputable imps))
        putText ")"
      IEGroup _ _ _ ->
        error $
          "Language.Haskell.Stylish.Printer.Imports.printImportExport: unhandled case 'IEGroup'" <> showOutputable export
      IEDoc _ _ ->
        error $
          "Language.Haskell.Stylish.Printer.Imports.printImportExport: unhandled case 'IEDoc'" <> showOutputable export
      IEDocNamed _ _ ->
        error $
          "Language.Haskell.Stylish.Printer.Imports.printImportExport: unhandled case 'IEDocNamed'" <> showOutputable export
      XIE ext ->
        GHC.noExtCon ext
