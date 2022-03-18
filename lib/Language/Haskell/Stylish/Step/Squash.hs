--------------------------------------------------------------------------------
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
module Language.Haskell.Stylish.Step.Squash
    ( step
    ) where


--------------------------------------------------------------------------------
import           Data.Maybe                      (listToMaybe)
import qualified GHC.Hs                          as GHC
import qualified GHC.Types.SrcLoc                as GHC


--------------------------------------------------------------------------------
import qualified Language.Haskell.Stylish.Editor as Editor
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Util


--------------------------------------------------------------------------------
-- | Removes anything between two RealSrcSpans, providing they are on the same
-- line.
squash :: GHC.RealSrcSpan -> GHC.RealSrcSpan -> Editor.Edits
squash l r
    | GHC.srcSpanEndLine l /= GHC.srcSpanStartLine r = mempty
    | GHC.srcSpanEndCol l >= GHC.srcSpanStartCol r = mempty
    | otherwise = Editor.replace
        (GHC.srcSpanEndLine l)
        (GHC.srcSpanEndCol l)
        (GHC.srcSpanStartCol r)
        " "


--------------------------------------------------------------------------------
squashFieldDecl :: GHC.ConDeclField GHC.GhcPs -> Editor.Edits
squashFieldDecl (GHC.ConDeclField ext names@(_ : _) type' _)
    | Just left <- GHC.srcSpanToRealSrcSpan . GHC.getLoc $ last names
    , Just sep <- fieldDeclSeparator ext
    , Just right <- GHC.srcSpanToRealSrcSpan $ GHC.getLocA type' =
        squash left sep <> squash sep right
squashFieldDecl _ = mempty


--------------------------------------------------------------------------------
fieldDeclSeparator :: GHC.EpAnn [GHC.AddEpAnn]-> Maybe GHC.RealSrcSpan
fieldDeclSeparator GHC.EpAnn {..} = listToMaybe $ do
    GHC.AddEpAnn GHC.AnnDcolon (GHC.EpaSpan s) <- anns
    pure s
fieldDeclSeparator _ = Nothing


--------------------------------------------------------------------------------
squashMatch
    :: GHC.LMatch GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> Editor.Edits
squashMatch lmatch = case GHC.m_grhss match of
    GHC.GRHSs _ [lgrhs] _
        | GHC.GRHS ext [] body <- GHC.unLoc lgrhs
        , Just left <- mbLeft
        , Just sep <- matchSeparator ext
        , Just right <- GHC.srcSpanToRealSrcSpan $ GHC.getLocA body ->
            squash left sep <> squash sep right
    _ -> mempty
  where
    match = GHC.unLoc lmatch
    mbLeft = case match of
        GHC.Match _ (GHC.FunRhs name _ _) [] _ ->
            GHC.srcSpanToRealSrcSpan $ GHC.getLocA name
        GHC.Match _ _ pats@(_ : _) _ ->
            GHC.srcSpanToRealSrcSpan . GHC.getLocA $ last pats
        _ -> Nothing


--------------------------------------------------------------------------------
matchSeparator :: GHC.EpAnn GHC.GrhsAnn -> Maybe GHC.RealSrcSpan
matchSeparator GHC.EpAnn {..}
    | GHC.AddEpAnn _ (GHC.EpaSpan s) <- GHC.ga_sep anns = Just s
matchSeparator _ = Nothing


--------------------------------------------------------------------------------
step :: Step
step = makeStep "Squash" $ \ls (module') ->
    let changes =
            foldMap squashFieldDecl (everything module') <>
            foldMap squashMatch (everything module') in
    Editor.apply changes ls
