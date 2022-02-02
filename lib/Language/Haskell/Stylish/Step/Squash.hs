--------------------------------------------------------------------------------
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE TypeFamilies          #-}
module Language.Haskell.Stylish.Step.Squash
    ( step
    ) where


--------------------------------------------------------------------------------
import           Control.Monad    (guard)
import           Data.Maybe       (mapMaybe)
import qualified GHC.Types.SrcLoc as GHC
import qualified GHC.Hs           as GHC


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Editor
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Util


--------------------------------------------------------------------------------
squash :: GHC.SrcSpan -> GHC.SrcSpan -> Maybe (Change String)
squash left right = do
  l <- GHC.srcSpanToRealSrcSpan left
  r <- GHC.srcSpanToRealSrcSpan right
  guard $
      GHC.srcSpanEndLine l == GHC.srcSpanStartLine r ||
      GHC.srcSpanEndLine l + 1 == GHC.srcSpanStartLine r
  pure $ changeLine (GHC.srcSpanEndLine l) $ \str ->
      let (pre, post) = splitAt (GHC.srcSpanEndCol l) str
      in [trimRight pre ++ " " ++ trimLeft post]


--------------------------------------------------------------------------------
squashFieldDecl :: GHC.ConDeclField GHC.GhcPs -> Maybe (Change String)
squashFieldDecl (GHC.ConDeclField _ names type' _)
  | null names = Nothing
  | otherwise  = squash (GHC.getLoc $ last names) (GHC.getLocA type')


--------------------------------------------------------------------------------
squashMatch
    :: GHC.Match GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> Maybe (Change String)
squashMatch (GHC.Match _ (GHC.FunRhs name _ _) [] grhss) = do
    body <- unguardedRhsBody grhss
    squash (GHC.getLocA name) (GHC.getLocA body)
squashMatch (GHC.Match _ _ pats grhss) = do
    body <- unguardedRhsBody grhss
    squash (GHC.getLocA $ last pats) (GHC.getLocA body)


--------------------------------------------------------------------------------
step :: Step
step = makeStep "Squash" $ \ls (module') ->
    let changes =
            mapMaybe squashFieldDecl (everything module') ++
            mapMaybe squashMatch (everything module') in
    applyChanges changes ls
