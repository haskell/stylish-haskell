{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module Language.Haskell.Stylish.Printer
  ( Printer(..)
  , PrinterConfig(..)
  , PrinterState(..)

    -- * Alias
  , P

    -- * Functions to use the printer
  , runPrinter
  , runPrinter_

    -- ** Combinators
  , comma
  , dot
  , getAnnot
  , getCurrentLineLength
  , getDocstrPrev
  , indent
  , newline
  , parenthesize
  , peekNextCommentPos
  , prefix
  , putComment
  , putEolComment
  , putOutputable
  , putType
  , putRdrName
  , putText
  , removeCommentTo
  , removeCommentToEnd
  , removeLineComment
  , sep
  , sortedAttachedComments
  , space
  , spaces
  , suffix

    -- ** Outputable helpers
  , showOutputable
  , compareOutputable
  ) where

--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.GHC (baseDynFlags)

--------------------------------------------------------------------------------
import           ApiAnnotation                   (AnnKeywordId(..), AnnotationComment(..))
import           GHC.Hs.Extension                (GhcPs, NoExtField(..))
import           GHC.Hs.Types                    (HsType(..))
import           Module                          (ModuleName, moduleNameString)
import           RdrName                         (RdrName(..))
import           SrcLoc                          (GenLocated(..), RealLocated)
import           SrcLoc                          (Located, SrcSpan(..))
import           SrcLoc                          (srcSpanStartLine, srcSpanEndLine)
import           Outputable                      (Outputable)
import qualified Outputable                      as GHC

--------------------------------------------------------------------------------
import           Control.Monad                   (forM_, replicateM, replicateM_)
import           Control.Monad.Reader            (MonadReader, ReaderT(..))
import           Control.Monad.State             (MonadState, State)
import           Control.Monad.State             (runState)
import           Control.Monad.State             (gets, modify)
import           Data.Foldable                   (find)
import           Data.Functor                    ((<&>))
import           Data.List                       (delete, isPrefixOf)
import           Data.List.NonEmpty              (NonEmpty(..))
import qualified Data.List.NonEmpty              as NonEmpty
import           Prelude                         hiding (lines)

--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Module (Module, lookupAnnotation)
import           Language.Haskell.Stylish.GHC    (unLocated)

type P = Printer
type Lines = [String]

newtype Printer a = Printer (ReaderT PrinterConfig (State PrinterState) a)
  deriving (Applicative, Functor, Monad, MonadReader PrinterConfig, MonadState PrinterState)

data PrinterConfig = PrinterConfig

data PrinterState = PrinterState
  { lines :: Lines
  , linePos :: !Int
  , currentLine :: String
  , pendingComments :: [RealLocated AnnotationComment]
  , parsedModule :: Module
  }

runPrinter :: PrinterConfig -> [RealLocated AnnotationComment] -> Module -> Printer a -> (a, Lines)
runPrinter cfg comments m (Printer printer) =
  let
    (a, PrinterState parsedLines _ startedLine _ _) = runReaderT printer cfg `runState` PrinterState [] 0 "" comments m
  in
    (a, parsedLines <> if startedLine == [] then [] else [startedLine])

runPrinter_ :: PrinterConfig -> [RealLocated AnnotationComment] -> Module -> Printer a -> Lines
runPrinter_ cfg comments m printer = snd (runPrinter cfg comments m printer)

putText :: String -> P ()
putText txt = do
  l <- gets currentLine
  modify \s -> s { currentLine = l <> txt }

putOutputable :: Outputable a => a -> P ()
putOutputable = putText . showOutputable

putComment :: AnnotationComment -> P ()
putComment = \case
  AnnLineComment s -> putText s
  AnnDocCommentNext s -> putText s
  AnnDocCommentPrev s -> putText s
  AnnDocCommentNamed s -> putText s
  AnnDocSection _ s -> putText s
  AnnDocOptions s -> putText s
  AnnBlockComment s -> putText s

-- | Given the current start line of 'SrcSpan', remove and put EOL comment for same line
putEolComment :: SrcSpan -> P ()
putEolComment = \case
  RealSrcSpan rspan -> do
    cmt <- removeComment \case
      L rloc (AnnLineComment s) ->
        and
          [ srcSpanStartLine rspan == srcSpanStartLine rloc
          , not ("-- ^" `isPrefixOf` s)
          , not ("-- |" `isPrefixOf` s)
          ]
      _ -> False
    forM_ cmt (\c -> space >> putComment c)
  UnhelpfulSpan _ -> pure ()

putRdrName :: Located RdrName -> P ()
putRdrName (L pos n) = case n of
  Unqual name -> do
    annots <- getAnnot pos
    if AnnOpenP `elem` annots then do
      putText "("
      putText (showOutputable name)
      putText ")"
    else if AnnBackquote `elem` annots then do
      putText "`"
      putText (showOutputable name)
      putText "`"
    else if AnnSimpleQuote `elem` annots then do
      putText "'"
      putText (showOutputable name)
    else
      putText (showOutputable name)
  Qual modulePrefix name ->
    putModulePrefix modulePrefix >> dot >> putText (showOutputable name)
  Orig _ name ->
    putText (showOutputable name)
  Exact name ->
    putText (showOutputable name)

getDocstrPrev :: SrcSpan -> P (Maybe AnnotationComment)
getDocstrPrev = \case
  UnhelpfulSpan _ -> pure Nothing
  RealSrcSpan rspan -> do
    removeComment \case
      L rloc (AnnLineComment s) ->
        and
          [ srcSpanStartLine rspan == srcSpanStartLine rloc
          , "-- ^" `isPrefixOf` s
          ]
      _ -> False


putModulePrefix :: ModuleName -> P ()
putModulePrefix = putText . moduleNameString

putType :: Located (HsType GhcPs) -> P ()
putType ltp = case unLocated ltp of
  HsFunTy NoExtField argTp funTp -> do
    putOutputable argTp
    space
    putText "->"
    space
    putType funTp
  HsAppTy NoExtField t1 t2 ->
    putType t1 >> space >> putType t2
  HsExplicitListTy NoExtField _ xs -> do
    putText "'["
    sep
      (comma >> space)
      (fmap putType xs)
    putText "]"
  HsExplicitTupleTy NoExtField xs -> do
    putText "'("
    sep
      (comma >> space)
      (fmap putType xs)
    putText ")"
  HsOpTy NoExtField lhs op rhs -> do
    putType lhs
    space
    putRdrName op
    space
    putType rhs
  HsTyVar NoExtField _ rdrName ->
    putRdrName rdrName
  HsTyLit _ tp ->
    putOutputable tp
  HsParTy _ tp -> do
    putText "("
    putType tp
    putText ")"
  HsTupleTy NoExtField _ xs -> do
    putText "("
    sep
      (comma >> space)
      (fmap putType xs)
    putText ")"
  HsForAllTy NoExtField _ _ _ ->
    putOutputable ltp
  HsQualTy NoExtField _ _ ->
    putOutputable ltp
  HsAppKindTy _ _ _ ->
    putOutputable ltp
  HsListTy _ _ ->
    putOutputable ltp
  HsSumTy _ _ ->
    putOutputable ltp
  HsIParamTy _ _ _ ->
    putOutputable ltp
  HsKindSig _ _ _ ->
    putOutputable ltp
  HsStarTy _ _ ->
    putOutputable ltp
  HsSpliceTy _ _ ->
    putOutputable ltp
  HsDocTy _ _ _ ->
    putOutputable ltp
  HsBangTy _ _ _ ->
    putOutputable ltp
  HsRecTy _ _ ->
    putOutputable ltp
  HsWildCardTy _ ->
    putOutputable ltp
  XHsType _ ->
    putOutputable ltp

newline :: P ()
newline = do
  l <- gets currentLine
  modify \s -> s { currentLine = "", linePos = 0, lines = lines s <> [l] }

space :: P ()
space = putText " "

spaces :: Int -> P ()
spaces i = replicateM_ i space

dot :: P ()
dot = putText "."

comma :: P ()
comma = putText ","

parenthesize :: P a -> P a
parenthesize action = putText "(" *> action <* putText ")"

sep :: P a -> [P a] -> P ()
sep _ [] = pure ()
sep s (first : rest) = first >> forM_ rest ((>>) s)

prefix :: P a -> P b -> P b
prefix pa pb = pa >> pb

suffix :: P a -> P b -> P a
suffix pa pb = pb >> pa

indent :: Int -> P a -> P a
indent i = (>>) (replicateM i space)

showOutputable :: GHC.Outputable a => a -> String
showOutputable = GHC.showPpr baseDynFlags

compareOutputable :: GHC.Outputable a => a -> a -> Ordering
compareOutputable i0 i1 = compare (showOutputable i0) (showOutputable i1)

-- | Gets comment on supplied 'line' and removes it from the state
removeLineComment :: Int -> P (Maybe AnnotationComment)
removeLineComment line =
  removeComment (\(L rloc _) -> srcSpanStartLine rloc == line)

-- | Removes comments from the state up to 'line' and returns the ones that were removed
removeCommentTo :: SrcSpan -> P [AnnotationComment]
removeCommentTo = \case
  UnhelpfulSpan _ -> pure []
  RealSrcSpan rspan -> removeCommentTo' (srcSpanStartLine rspan)

removeCommentToEnd :: SrcSpan -> P [AnnotationComment]
removeCommentToEnd = \case
  UnhelpfulSpan _ -> pure []
  RealSrcSpan rspan -> removeCommentTo' (srcSpanEndLine rspan)

removeCommentTo' :: Int -> P [AnnotationComment]
removeCommentTo' line =
  removeComment (\(L rloc _) -> srcSpanStartLine rloc < line) >>= \case
    Nothing -> pure []
    Just c -> do
      rest <- removeCommentTo' line
      pure (c : rest)

-- | Remove a comment from the state given predicate 'p'
removeComment :: (RealLocated AnnotationComment -> Bool) -> P (Maybe AnnotationComment)
removeComment p = do
  comments <- gets pendingComments

  let
    foundComment =
      find p comments

    newPendingComments =
      maybe comments (`delete` comments) foundComment

  modify \s -> s { pendingComments = newPendingComments }
  pure $ fmap (\(L _ c) -> c) foundComment

getAnnot :: SrcSpan -> P [AnnKeywordId]
getAnnot spn = gets (lookupAnnotation spn . parsedModule)

getCurrentLineLength :: P Int
getCurrentLineLength = fmap length (gets currentLine)

peekNextCommentPos :: P (Maybe SrcSpan)
peekNextCommentPos = do
  gets pendingComments <&> \case
    (L next _ : _) -> Just (RealSrcSpan next)
    [] -> Nothing

sortedAttachedComments :: Outputable a => [Located a] -> P [([AnnotationComment], NonEmpty (Located a))]
sortedAttachedComments origs = go origs <&> fmap sortGroup
  where
    sortGroup = fmap (NonEmpty.sortBy compareOutputable)

    go :: [Located a] -> P [([AnnotationComment], NonEmpty (Located a))]
    go (L rspan x : xs) = do
      comments <- removeCommentTo rspan
      nextGroupStartM <- peekNextCommentPos

      let
        sameGroupOf = maybe xs \nextGroupStart ->
          takeWhile (\(L p _)-> p < nextGroupStart) xs

        restOf = maybe [] \nextGroupStart ->
          dropWhile (\(L p _) -> p <= nextGroupStart) xs

      restGroups <- go (restOf nextGroupStartM)
      pure $ (comments, L rspan x :| sameGroupOf nextGroupStartM) : restGroups

    go _ = pure []
