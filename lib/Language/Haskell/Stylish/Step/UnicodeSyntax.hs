--------------------------------------------------------------------------------
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Language.Haskell.Stylish.Step.UnicodeSyntax
    ( step
    ) where


--------------------------------------------------------------------------------
import qualified GHC.Hs                                        as GHC
import qualified GHC.Types.SrcLoc                              as GHC


--------------------------------------------------------------------------------
import qualified Language.Haskell.Stylish.Editor               as Editor
import           Language.Haskell.Stylish.Module
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Step.LanguagePragmas (addLanguagePragma)
import           Language.Haskell.Stylish.Util                 (everything)

--------------------------------------------------------------------------------
hsArrowReplacements :: GHC.HsArrow GHC.GhcPs -> Editor.Edits
hsArrowReplacements = \case
    GHC.HsUnrestrictedArrow (GHC.L (GHC.TokenLoc l) GHC.HsNormalTok) ->
        Editor.replaceRealSrcSpan (GHC.epaLocationRealSrcSpan l) "→"
    GHC.HsLinearArrow (GHC.HsPct1 _ (GHC.L (GHC.TokenLoc l) GHC.HsNormalTok)) ->
        Editor.replaceRealSrcSpan (GHC.epaLocationRealSrcSpan l) "→"
    GHC.HsExplicitMult _ _ (GHC.L (GHC.TokenLoc l) GHC.HsNormalTok) ->
        Editor.replaceRealSrcSpan (GHC.epaLocationRealSrcSpan l) "→"
    _ -> mempty


--------------------------------------------------------------------------------
hsAnnContextReplacements :: GHC.AnnContext -> Editor.Edits
hsAnnContextReplacements (GHC.AnnContext{GHC.ac_darrow})
    | Just (GHC.NormalSyntax, GHC.EpaSpan loc) <- ac_darrow =
        Editor.replaceRealSrcSpan loc "⇒"
    | otherwise = mempty


--------------------------------------------------------------------------------
hsAddEpAnnReplacements :: GHC.AddEpAnn -> Editor.Edits
hsAddEpAnnReplacements (GHC.AddEpAnn el (GHC.EpaSpan loc)) = case el of
    GHC.AnnDcolon -> Editor.replaceRealSrcSpan loc "∷"
    GHC.AnnForall -> Editor.replaceRealSrcSpan loc "∀"
    _ -> mempty
hsAddEpAnnReplacements _ = mempty


--------------------------------------------------------------------------------
step :: Bool -> String -> Step
step = (makeStep "UnicodeSyntax" .) . step'


--------------------------------------------------------------------------------
step' :: Bool -> String -> Lines -> Module -> Lines
step' alp lg ls modu = Editor.apply edits ls
  where
    edits = foldMap hsArrowReplacements  (everything  modu) <>
        foldMap hsAnnContextReplacements (everything modu) <>
        foldMap hsAddEpAnnReplacements (everything modu) <>
        (if alp then addLanguagePragma lg "UnicodeSyntax" modu else mempty)
