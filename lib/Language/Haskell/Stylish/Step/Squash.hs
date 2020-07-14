--------------------------------------------------------------------------------
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE TypeFamilies          #-}
module Language.Haskell.Stylish.Step.Squash
    ( step
    ) where


--------------------------------------------------------------------------------
import           Data.Maybe                      (mapMaybe)
import qualified GHC.Hs                          as Hs
import qualified GHC.Hs.Extension                as E
import qualified GHC.Hs.Types                    as T
import qualified SrcLoc                          as S


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Editor
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Util


--------------------------------------------------------------------------------
squash
    :: (S.HasSrcSpan l, S.HasSrcSpan r)
    => l -> r -> Maybe (Change String)
squash left right = do
  lAnn <- toRealSrcSpan $ S.getLoc left
  rAnn <- toRealSrcSpan $ S.getLoc right
  if S.srcSpanEndLine lAnn == S.srcSpanStartLine rAnn ||
      S.srcSpanEndLine lAnn + 1 == S.srcSpanStartLine rAnn
    then Just $
          changeLine (S.srcSpanEndLine lAnn) $ \str ->
          let (pre, post) = splitAt (S.srcSpanEndCol lAnn) str
          in [trimRight pre ++ " " ++ trimLeft post]
    else Nothing

toRealSrcSpan :: S.SrcSpan -> Maybe S.RealSrcSpan
toRealSrcSpan (S.RealSrcSpan s) = Just s
toRealSrcSpan _                 = Nothing


--------------------------------------------------------------------------------
squashFieldDecl :: T.ConDeclField E.GhcPs -> Maybe (Change String)
squashFieldDecl (T.ConDeclField _ names type' _)
  | null names = Nothing
  | otherwise  = squash (last names) type'
squashFieldDecl (T.XConDeclField x) = E.noExtCon x


--------------------------------------------------------------------------------
-- Utility: grab the body out of guarded RHSs if it's a single unguarded one.
unguardedRhsBody :: Hs.GRHSs E.GhcPs a -> Maybe a
unguardedRhsBody (Hs.GRHSs _ [grhs] _)
    | Hs.GRHS _ [] body <- S.unLoc grhs = Just body
unguardedRhsBody _ = Nothing


--------------------------------------------------------------------------------
squashMatch :: Hs.Match E.GhcPs (Hs.LHsExpr E.GhcPs) -> Maybe (Change String)
squashMatch (Hs.Match _ (Hs.FunRhs name _ _) [] grhss) = do
    body <- unguardedRhsBody grhss
    squash name body
squashMatch (Hs.Match _ _ pats grhss) = do
    body <- unguardedRhsBody grhss
    squash (last pats) body
squashMatch (Hs.XMatch x) = E.noExtCon x


--------------------------------------------------------------------------------
step :: Step
step = makeStep "Squash" $ Right $ \ls (module') ->
    let changes =
            mapMaybe squashFieldDecl (everything module') ++
            mapMaybe squashMatch (everything module') in
    applyChanges changes ls
