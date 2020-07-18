{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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
  , newline
  , parenthesize
  , prefix
  , putComment
  , putText
  , sep
  , space
  , spaces
  , suffix
  , indent
  , peekNextCommentPos
  , removeLineComment
  , removeCommentTo
  , sortedAttachedComments

    -- ** Outputable helpers
  , showOutputable
  , compareOutputable
  ) where

--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Parse  (baseDynFlags)

--------------------------------------------------------------------------------
import           ApiAnnotation                   (AnnotationComment(..))
import           SrcLoc                          (GenLocated(..), RealLocated)
import           SrcLoc                          (Located, SrcSpan(..))
import           SrcLoc                          (srcSpanStartLine)
import           Control.Monad                   (forM_, replicateM, replicateM_)
import           Control.Monad.Reader            (MonadReader, ReaderT(..))
import           Control.Monad.State             (MonadState, State)
import           Control.Monad.State             (runState)
import           Control.Monad.State             (gets, modify)
import           Data.Foldable                   (find)
import           Data.Functor                    ((<&>))
import           Data.List                       (delete)
import           Data.List.NonEmpty              (NonEmpty(..))
import qualified Data.List.NonEmpty              as NonEmpty
import           GHC.Generics                    (Generic)
import           Outputable                      (Outputable)
import qualified Outputable                      as GHC
import           Prelude                         hiding (lines)

--------------------------------------------------------------------------------

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
  }
  deriving stock (Generic)

runPrinter :: PrinterConfig -> [RealLocated AnnotationComment] -> Printer a -> (a, Lines)
runPrinter cfg comments (Printer printer) =
  let
    (a, PrinterState parsedLines _ startedLine _) = runReaderT printer cfg `runState` PrinterState [] 0 "" comments
  in
    (a, parsedLines <> if startedLine == [] then [] else [startedLine])

runPrinter_ :: PrinterConfig -> [RealLocated AnnotationComment] -> Printer a -> Lines
runPrinter_ cfg comments printer = snd (runPrinter cfg comments printer)

putText :: String -> P ()
putText txt = do
  l <- gets currentLine
  modify \s -> s { currentLine = l <> txt }

putComment :: AnnotationComment -> P ()
putComment = \case
  AnnLineComment s -> putText s
  AnnDocCommentNext s -> putText s
  AnnDocCommentPrev s -> putText s
  AnnDocCommentNamed s -> putText s
  AnnDocSection _ s -> putText s
  AnnDocOptions s -> putText s
  AnnBlockComment s -> putText s

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
removeCommentTo :: Int -> P [AnnotationComment]
removeCommentTo line =
  removeComment (\(L rloc _) -> srcSpanStartLine rloc < line) >>= \case
    Nothing -> pure []
    Just c -> do
      rest <- removeCommentTo line
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
    go (L (RealSrcSpan rloc) x : xs) = do
      comments <- removeCommentTo (srcSpanStartLine rloc)
      nextGroupStartM <- peekNextCommentPos

      let
        sameGroupOf = maybe xs \nextGroupStart ->
          takeWhile (\(L p _)-> p < nextGroupStart) xs

        restOf = maybe [] \nextGroupStart ->
          dropWhile (\(L p _) -> p <= nextGroupStart) xs

      restGroups <- go (restOf nextGroupStartM)
      pure $ (comments, L (RealSrcSpan rloc) x :| sameGroupOf nextGroupStartM) : restGroups

    go _ = pure []
