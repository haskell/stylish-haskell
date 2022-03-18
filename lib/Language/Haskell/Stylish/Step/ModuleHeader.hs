{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Language.Haskell.Stylish.Step.ModuleHeader
  ( Config (..)
  , BreakWhere (..)
  , OpenBracket (..)
  , defaultConfig
  , step
  ) where


--------------------------------------------------------------------------------
import           Control.Applicative                   ((<|>))
import           Control.Monad                         (guard, unless, when)
import           Data.Foldable                         (forM_)
import           Data.Maybe                            (fromMaybe, isJust,
                                                        listToMaybe)
import qualified GHC.Hs                                as GHC
import qualified GHC.Types.SrcLoc                      as GHC
import qualified GHC.Unit.Module.Name                  as GHC


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Comments
import qualified Language.Haskell.Stylish.Editor       as Editor
import           Language.Haskell.Stylish.GHC
import           Language.Haskell.Stylish.Module
import           Language.Haskell.Stylish.Ordering
import           Language.Haskell.Stylish.Printer
import           Language.Haskell.Stylish.Step
import qualified Language.Haskell.Stylish.Step.Imports as Imports
import           Language.Haskell.Stylish.Util         (flagEnds)


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
printModuleHeader maxCols conf ls lmodul =
    let modul = GHC.unLoc lmodul
        name = GHC.unLoc <$> GHC.hsmodName modul
        haddocks = GHC.hsmodHaddockModHeader modul

        startLine = fromMaybe 1 $ moduleLine <|>
            (fmap GHC.srcSpanStartLine . GHC.srcSpanToRealSrcSpan $
                GHC.getLoc lmodul)

        endLine = fromMaybe 1 $ whereLine <|>
            (do
                loc <- GHC.getLocA <$> GHC.hsmodExports modul
                GHC.srcSpanEndLine <$> GHC.srcSpanToRealSrcSpan loc)

        keywordLine kw = listToMaybe $ do
            GHC.EpAnn {..} <- pure $ GHC.hsmodAnn modul
            GHC.AddEpAnn kw' (GHC.EpaSpan s) <- GHC.am_main anns
            guard $ kw == kw'
            pure $ GHC.srcSpanEndLine s

        moduleLine = keywordLine GHC.AnnModule
        whereLine = keywordLine GHC.AnnWhere

        commentOnLine l = listToMaybe $ do
            comment <- epAnnComments $ GHC.hsmodAnn modul
            guard $ GHC.srcSpanStartLine (GHC.anchor $ GHC.getLoc comment) == l
            pure comment

        moduleComment = moduleLine >>= commentOnLine
        whereComment =
            guard (whereLine /= moduleLine) >> whereLine >>= commentOnLine

        exportGroups = case GHC.hsmodExports modul of
            Nothing -> Nothing
            Just lexports -> Just $ doSort $ commentGroups
                (GHC.srcSpanToRealSrcSpan . GHC.getLocA)
                (GHC.unLoc lexports)
                (epAnnComments . GHC.ann $ GHC.getLoc lexports)

        printedModuleHeader = runPrinter_
            (PrinterConfig maxCols)
            (printHeader
                conf name exportGroups haddocks moduleComment whereComment)

        changes = Editor.changeLines
            (Editor.Block startLine endLine)
            (const printedModuleHeader) in

    Editor.apply changes ls
  where
    doSort = if sort conf then fmap (commentGroupSort compareLIE) else id

printHeader
    :: Config
    -> Maybe GHC.ModuleName
    -> Maybe [CommentGroup (GHC.LIE GHC.GhcPs)]
    -> Maybe GHC.LHsDocString
    -> Maybe GHC.LEpaComment  -- Comment attached to 'module'
    -> Maybe GHC.LEpaComment  -- Comment attached to 'where'
    -> P ()
printHeader conf mbName mbExps _ mbModuleComment mbWhereComment = do
    forM_ mbName $ \name -> do
        putText "module"
        space
        putText (showOutputable name)

    case mbExps of
        Nothing -> do
            when (isJust mbName) $ case breakWhere conf of
                Always -> do
                    attachModuleComment
                    newline
                    spaces (indent conf)
                _      -> space
            putText "where"
        Just exports -> case breakWhere conf of
            Single  | [] <- exports -> do
                printSingleLineExportList conf []
                attachModuleComment
            Single  | [egroup] <- exports
                    , not (commentGroupHasComments egroup)
                    , [(export, _)] <- (cgItems egroup) -> do
                printSingleLineExportList conf [export]
                attachModuleComment
            Inline  | [] <- exports -> do
                printSingleLineExportList conf []
                attachModuleComment
            Inline  | [egroup] <- exports, not (commentGroupHasComments egroup) -> do
                wrapping
                    (printSingleLineExportList conf $ map fst $ cgItems egroup)
                    (do
                        attachOpenBracket
                        attachModuleComment
                        printMultiLineExportList conf exports)
            _ -> do
                attachOpenBracket
                attachModuleComment
                printMultiLineExportList conf exports

    putMaybeLineComment $ GHC.unLoc <$> mbWhereComment
  where
    attachModuleComment = putMaybeLineComment $ GHC.unLoc <$> mbModuleComment

    attachOpenBracket
        | openBracket conf == SameLine = putText " ("
        | otherwise                    = pure ()

printSingleLineExportList
    :: Config -> [GHC.LIE GHC.GhcPs] -> P ()
printSingleLineExportList conf exports = do
    space >> putText "("
    printExports exports
    putText ")" >> space >> putText "where"
  where
    printExports :: [GHC.LIE GHC.GhcPs] -> P ()
    printExports = \case
        []     -> pure ()
        [e]    -> putExport conf e
        (e:es) -> putExport conf e >> comma >> space >> printExports es

printMultiLineExportList
     :: Config
     -> [CommentGroup (GHC.LIE GHC.GhcPs)]
     -> P ()
printMultiLineExportList conf exports = do
    newline
    doIndent >> putText firstChar >> unless (null exports) space
    mapM_ printExport $ flagEnds exports
    when (null exports) $ newline >> doIndent
    putText ")" >> space >> putText "where"
  where
    printExport (CommentGroup {..}, firstGroup, _lastGroup) = do
        forM_ (flagEnds cgPrior) $ \(cmt, start, _end) -> do
            unless (firstGroup && start) $ space >> space
            putComment $ GHC.unLoc cmt
            newline >> doIndent

        forM_ (flagEnds cgItems) $ \((export, mbComment), start, _end) -> do
            if firstGroup && start then
                unless (null cgPrior) $ space >> space
            else
                comma >> space
            putExport conf export
            putMaybeLineComment $ GHC.unLoc <$> mbComment
            newline >> doIndent

    firstChar = case openBracket conf of
        SameLine -> " "
        NextLine -> "("

    doIndent = spaces (indent conf)

-- NOTE(jaspervdj): This code is almost the same as the import printing in
-- 'Imports' and should be merged.
putExport :: Config -> GHC.LIE GHC.GhcPs -> P ()
putExport conf = Imports.printImport (separateLists conf) . GHC.unLoc
