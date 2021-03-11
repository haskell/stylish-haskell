{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase     #-}
module Language.Haskell.Stylish.Step.ModuleHeader
  ( Config (..)
  , BreakWhere (..)
  , OpenBracket (..)
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
    { indent        :: Int
    , sort          :: Bool
    , separateLists :: Bool
    , breakWhere    :: BreakWhere
    , openBracket   :: OpenBracket
    }

data OpenBracket
    = SameLine
    | NextLine
    deriving (Eq, Show)

data BreakWhere
    = Exports
    | Single
    | Inline
    | Always
    deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config
    { indent        = 4
    , sort          = True
    , separateLists = True
    , breakWhere    = Exports
    , openBracket   = NextLine
    }

step :: Maybe Int -> Config -> Step
step maxCols = makeStep "Module header" . printModuleHeader maxCols

printModuleHeader :: Maybe Int -> Config -> Lines -> Module -> Lines
printModuleHeader maxCols conf ls m =
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

    printedModuleHeader = runPrinter_ (PrinterConfig maxCols) relevantComments
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
  forM_ mname \(L _ name) -> do
    putText "module"
    space
    putText (showOutputable name)

  case mexps of
    Nothing -> when (isJust mname) do
      forM_ mname \(L nloc _) -> attachEolComment nloc
      case breakWhere conf of
        Always -> do
          newline
          spaces (indent conf)
        _      -> space
      putText "where"
    Just (L loc exps) -> do
      moduleComment <- getModuleComment
      exportsWithComments <- fmap (second doSort) <$> groupAttachedComments exps
      case breakWhere conf of
        Single
          | Just exportsWithoutComments <- groupWithoutComments exportsWithComments
          , length exportsWithoutComments <= 1
          -> do
              attachModuleComment moduleComment
              printSingleLineExportList conf (L loc exportsWithoutComments)
        Inline
          | Just exportsWithoutComments <- groupWithoutComments exportsWithComments
          -> do
              wrapping
               (   attachModuleComment moduleComment
                >> printSingleLineExportList conf (L loc exportsWithoutComments))
               (   attachOpenBracket
                >> attachModuleComment moduleComment
                >> printMultiLineExportList conf (L loc exportsWithComments))
        _ -> do
          attachOpenBracket
          attachModuleComment moduleComment
          printMultiLineExportList conf (L loc exportsWithComments)
  where

    getModuleComment = do
       maybemaybeComment <- traverse (\(L nloc _) -> removeModuleComment nloc) mname
       pure $ join maybemaybeComment

    attachModuleComment moduleComment =
      mapM_ (\c -> space >> putComment c) moduleComment

    doSort = if sort conf then NonEmpty.sortBy compareLIE else id

    attachOpenBracket
      | openBracket conf == SameLine = putText " ("
      | otherwise                    = pure ()

removeModuleComment :: SrcSpan -> P (Maybe AnnotationComment)
removeModuleComment = \case
  UnhelpfulSpan _ -> pure Nothing
  RealSrcSpan rspan ->
    removeLineComment (srcSpanStartLine rspan)

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

printSingleLineExportList :: Config -> Located [GHC.LIE GhcPs] -> P ()
printSingleLineExportList conf (L srcLoc exports) = do
  space >> putText "("
  printInlineExports exports
  putText ")" >> space >> putText "where" >> attachEolCommentEnd srcLoc
  where
    printInlineExports :: [GHC.LIE GhcPs] -> P ()
    printInlineExports = \case
      []     -> pure ()
      [e]    -> printExport conf e
      (e:es) -> printExport conf e >> comma >> space >> printInlineExports es

printMultiLineExportList
     :: Config
     -> Located [([AnnotationComment], NonEmpty (GHC.LIE GhcPs))]
     -> P ()
printMultiLineExportList conf (L srcLoc exportsWithComments) = do
  newline
  doIndent >> putText firstChar >> when (notNull exportsWithComments) space
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

    firstChar =
      case openBracket conf of
        SameLine -> " "
        NextLine -> "("

    doIndent = spaces (indent conf)
    doHang = pad (indent conf + 2)

    printExports :: [([AnnotationComment], NonEmpty (GHC.LIE GhcPs))] -> P ()
    printExports (([], firstInGroup :| groupRest) : rest) = do
      printExport conf firstInGroup
      newline
      doIndent
      printExportsGroupTail groupRest
      printExportsTail rest
    printExports ((firstComment : comments, firstExport :| groupRest) : rest) = do
      putComment firstComment >> newline >> doIndent
      forM_ comments \c -> doHang >> putComment c >> newline >> doIndent
      doHang
      printExport conf firstExport
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
        comma >> space >> printExport conf export
        newline >> doIndent

    printExportsGroupTail :: [GHC.LIE GhcPs] -> P ()
    printExportsGroupTail (x : xs) = printExportsTail [([], x :| xs)]
    printExportsGroupTail []       = pure ()

-- NOTE(jaspervdj): This code is almost the same as the import printing in
-- 'Imports' and should be merged.
printExport :: Config -> GHC.LIE GhcPs -> P ()
printExport conf = Imports.printImport (separateLists conf) . unLoc
