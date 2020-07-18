{-# LANGUAGE LambdaCase #-}
-- | Utility functions for working with the GHC AST
module Language.Haskell.Stylish.GHC
  ( dropAfterLocated
  , dropBeforeLocated
  , getEndLineUnsafe
  , getStartLineUnsafe
  ) where

--------------------------------------------------------------------------------
import           SrcLoc                        (GenLocated(..), SrcSpan(..))
import           SrcLoc                        (Located, RealLocated)
import           SrcLoc                        (srcSpanStartLine, srcSpanEndLine)

getStartLineUnsafe :: Located a -> Int
getStartLineUnsafe = \case
  (L (RealSrcSpan s) _) -> srcSpanStartLine s
  _ -> error "could not get start line of block"

getEndLineUnsafe :: Located a -> Int
getEndLineUnsafe = \case
  (L (RealSrcSpan s) _) -> srcSpanEndLine s
  _ -> error "could not get end line of block"

dropAfterLocated :: Maybe (Located a) -> [RealLocated b] -> [RealLocated b]
dropAfterLocated loc xs = case loc of
  Just (L (RealSrcSpan rloc) _) ->
    filter (\(L x _) -> srcSpanEndLine rloc >= srcSpanStartLine x) xs
  _ -> xs

dropBeforeLocated :: Maybe (Located a) -> [RealLocated b] -> [RealLocated b]
dropBeforeLocated loc xs = case loc of
  Just (L (RealSrcSpan rloc) _) ->
    filter (\(L x _) -> srcSpanStartLine rloc <= srcSpanEndLine x) xs
  _ -> xs
