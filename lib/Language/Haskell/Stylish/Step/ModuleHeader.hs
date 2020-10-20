{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase     #-}
module Language.Haskell.Stylish.Step.ModuleHeader
  ( Config (..)
  , defaultConfig
  , step
  ) where

--------------------------------------------------------------------------------
import           ApiAnnotation                         (AnnKeywordId (..),
                                                        AnnotationComment (..))
import           Control.Monad                         (forM_, join, when)
import           Data.Bifunctor                        (second)
import           Data.Foldable                         (find, toList)
import           Data.Function                         ((&))
import qualified Data.List                             as L
import           Data.List.NonEmpty                    (NonEmpty (..))
import qualified Data.List.NonEmpty                    as NonEmpty
import           Data.Maybe                            (isJust, listToMaybe)
import qualified GHC.Hs.Doc                            as GHC
import           GHC.Hs.Extension                      (GhcPs)
import qualified GHC.Hs.ImpExp                         as GHC
import qualified Module                                as GHC
import           SrcLoc                                (GenLocated (..),
                                                        Located, RealLocated,
                                                        SrcSpan (..),
                                                        srcSpanEndLine,
                                                        srcSpanStartLine, unLoc)
import           Util                                  (notNull)

--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Block
import           Language.Haskell.Stylish.Editor
import           Language.Haskell.Stylish.GHC
import           Language.Haskell.Stylish.Module
import           Language.Haskell.Stylish.Ordering
import           Language.Haskell.Stylish.Printer
import           Language.Haskell.Stylish.Step
import qualified Language.Haskell.Stylish.Step.Imports as Imports


data Config = Config
    { indent         :: Int
    , sort           :: Bool
    , separateLists  :: Bool
    , breakOnlyWhere :: Bool
    }

defaultConfig :: Config
defaultConfig = Config
    { indent         = 4
    , sort           = True
    , separateLists  = True
    , breakOnlyWhere = False
    }

step :: Config -> Step
step = makeStep "Module header" . printModuleHeader

printModuleHeader :: Config -> Lines -> Module -> Lines
printModuleHeader conf ls m =
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
      & dropAfterLocated exports
      & dropBeforeLocated name

    -- TODO: pass max columns?
    printedModuleHeader = runPrinter_ (PrinterConfig Nothing) relevantComments
        m (printHeader conf name exports haddocks)

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
      & L.sort
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

printHeader
  :: Config
  -> Maybe (Located GHC.ModuleName)
  -> Maybe (Located [GHC.LIE GhcPs])
  -> Maybe GHC.LHsDocString
  -> P ()
printHeader conf mname mexps _ = do
  forM_ mname \(L loc name) -> do
    putText "module"
    space
    putText (showOutputable name)
    attachEolComment loc

  case mexps of
    Nothing -> when (isJust mname) do
      if breakOnlyWhere conf
        then do
          newline
          spaces (indent conf)
        else space
      putText "where"
    Just exps -> printExportList conf exps

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

printExportList :: Config -> Located [GHC.LIE GhcPs] -> P ()
printExportList conf (L srcLoc exports) = do
  newline
  doIndent >> putText "(" >> when (notNull exports) space

  exportsWithComments <- fmap (second doSort) <$> groupAttachedComments exports

  printExports exportsWithComments

  putText ")" >> space >> putText "where" >> attachEolCommentEnd srcLoc
  where
    -- 'doIndent' is @x@:
    --
    -- > module Foo
    -- > xxxx( foo
    -- > xxxx, bar
    -- > xxxx) where
    --
    -- 'doHang' is @y@:
    --
    -- > module Foo
    -- > xxxx( -- Some comment
    -- > xxxxyyfoo
    -- > xxxx) where
    doIndent = spaces (indent conf)
    doHang = pad (indent conf + 2)

    doSort = if sort conf then NonEmpty.sortBy compareLIE else id

    printExports :: [([AnnotationComment], NonEmpty (GHC.LIE GhcPs))] -> P ()
    printExports (([], firstInGroup :| groupRest) : rest) = do
      printExport firstInGroup
      newline
      doIndent
      printExportsGroupTail groupRest
      printExportsTail rest
    printExports ((firstComment : comments, firstExport :| groupRest) : rest) = do
      putComment firstComment >> newline >> doIndent
      forM_ comments \c -> doHang >> putComment c >> newline >> doIndent
      doHang
      printExport firstExport
      newline
      doIndent
      printExportsGroupTail groupRest
      printExportsTail rest
    printExports [] =
      newline >> doIndent

    printExportsTail :: [([AnnotationComment], NonEmpty (GHC.LIE GhcPs))] -> P ()
    printExportsTail = mapM_ \(comments, exported) -> do
      forM_ comments \c -> doHang >> putComment c >> newline >> doIndent
      forM_ exported \export -> do
        comma >> space >> printExport export
        newline >> doIndent

    printExportsGroupTail :: [GHC.LIE GhcPs] -> P ()
    printExportsGroupTail (x : xs) = printExportsTail [([], x :| xs)]
    printExportsGroupTail []       = pure ()

    -- NOTE(jaspervdj): This code is almost the same as the import printing
    -- in 'Imports' and should be merged.
    printExport :: GHC.LIE GhcPs -> P ()
    printExport = Imports.printImport (separateLists conf) . unLoc
