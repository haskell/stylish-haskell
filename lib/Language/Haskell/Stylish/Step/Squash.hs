--------------------------------------------------------------------------------
{-# LANGUAGE PartialTypeSignatures #-}
module Language.Haskell.Stylish.Step.Squash
    ( step
    ) where


--------------------------------------------------------------------------------
import qualified CoreSyn                         as C
import           Data.Maybe                      (mapMaybe)
import           Debug.Trace
import qualified GHC.Hs                          as Hs
import qualified GHC.Hs.Binds                    as B
import qualified GHC.Hs.Extension                as E
import qualified GHC.Hs.Types                    as T
import qualified Language.Haskell.Exts           as H
import qualified Outputable
import qualified SrcLoc                          as S

--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Editor
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Util


--------------------------------------------------------------------------------
squash
    :: (H.Annotated l, H.Annotated r)
    => l H.SrcSpan -> r H.SrcSpan -> Maybe (Change String)
squash left right
  | H.srcSpanEndLine lAnn == H.srcSpanStartLine rAnn = Just $
        changeLine (H.srcSpanEndLine lAnn) $ \str ->
        let (pre, post) = splitAt (H.srcSpanEndColumn lAnn) str
        in  [trimRight pre ++ " " ++ trimLeft post]
  | otherwise = Nothing
  where
    lAnn = H.ann left
    rAnn = H.ann right

squash'
    :: (S.HasSrcSpan l, S.HasSrcSpan r)
    => l -> r -> Maybe (Change String)
squash' left right = do
  traceM $ show $ S.getLoc right
  lAnn <- toRealSrcSpan $ S.getLoc left
  rAnn <- toRealSrcSpan $ S.getLoc right
  if S.srcSpanEndLine lAnn == S.srcSpanStartLine rAnn
    then Just $
          changeLine (S.srcSpanEndLine lAnn) $ \str ->
          let (pre, post) = splitAt (S.srcSpanEndCol lAnn) str
          in  trace ("pre:" ++ show pre) $
              trace ("post:" ++ show post) $
              [trimRight pre ++ " " ++ trimLeft post]
    else Nothing

toRealSrcSpan :: S.SrcSpan -> Maybe S.RealSrcSpan
toRealSrcSpan (S.RealSrcSpan s) = Just s
toRealSrcSpan _                 = Nothing


--------------------------------------------------------------------------------
squashFieldDecl :: H.FieldDecl H.SrcSpan -> Maybe (Change String)
squashFieldDecl (H.FieldDecl _ names type')
  | null names = Nothing
  | otherwise  = squash (last names) type'

squashFieldDecl' :: T.ConDeclField E.GhcPs -> Maybe (Change String)
squashFieldDecl' (T.ConDeclField _ names type' _)
  | null names = Nothing
  | otherwise  = squash' (last names) type'
squashFieldDecl' (T.XConDeclField _) = Nothing


--------------------------------------------------------------------------------
squashMatch :: H.Match H.SrcSpan -> Maybe (Change String)
squashMatch (H.InfixMatch _ _ _ _ _ _) = Nothing
squashMatch (H.Match _ name pats rhs _)
  | null pats = squash name        rhs
  | otherwise = squash (last pats) rhs


-- Utility: grab the body out of guarded RHSs if it's a single unguarded one.
unguardedRhsBody :: Hs.GRHSs E.GhcPs a -> Maybe a
unguardedRhsBody (Hs.GRHSs _ [grhs] _)
    | Hs.GRHS _ [] body <- S.unLoc grhs = Just body
unguardedRhsBody _ = Nothing


squashMatch' :: Hs.Match E.GhcPs (Hs.LHsExpr E.GhcPs) -> Maybe (Change String)
squashMatch' (Hs.Match _ (Hs.FunRhs name _ _) [] grhss) = do
    body <- unguardedRhsBody grhss
    squash' name body
squashMatch' (Hs.Match _ _ pats grhss) = do
    traceM "wat up"
    body <- unguardedRhsBody grhss
    squash' (last pats) body


--------------------------------------------------------------------------------
squashAlt :: H.Alt H.SrcSpan -> Maybe (Change String)
squashAlt (H.Alt _ pat rhs _) = squash pat rhs

squashAlt' :: C.Alt S.SrcSpan -> Maybe (Change String)
squashAlt' alt = undefined


--------------------------------------------------------------------------------
step :: Step
step = makeStep "Squash" $ Right $ \ls (module') ->
    let fieldDecls = everything module'
        ghcchanges = trace ("fieldDecls: " ++ show (length fieldDecls)) $
            mapMaybe squashFieldDecl' fieldDecls ++
            mapMaybe squashMatch' (everything module')

    in trace ("ghc changes: " ++ show (length ghcchanges)) $ applyChanges ghcchanges ls
