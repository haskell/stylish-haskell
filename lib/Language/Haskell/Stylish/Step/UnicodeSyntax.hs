--------------------------------------------------------------------------------
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
hsTyReplacements :: GHC.HsType GHC.GhcPs -> Editor.Edits
hsTyReplacements (GHC.HsFunTy xann arr _ _)
    | GHC.HsUnrestrictedArrow GHC.NormalSyntax <- arr
    , GHC.AddRarrowAnn (GHC.EpaSpan loc) <- GHC.anns xann =
        Editor.replaceRealSrcSpan loc "→"
hsTyReplacements (GHC.HsQualTy _ (Just ctx) _)
    | Just arrow <- GHC.ac_darrow . GHC.anns . GHC.ann $ GHC.getLoc ctx
    , (GHC.NormalSyntax, GHC.EpaSpan loc) <- arrow =
        Editor.replaceRealSrcSpan loc "⇒"
hsTyReplacements _ = mempty


--------------------------------------------------------------------------------
hsSigReplacements :: GHC.Sig GHC.GhcPs -> Editor.Edits
hsSigReplacements (GHC.TypeSig ann _ _)
    | GHC.AddEpAnn GHC.AnnDcolon epaLoc <- GHC.asDcolon $ GHC.anns ann
    , GHC.EpaSpan loc <- epaLoc =
        Editor.replaceRealSrcSpan loc "∷"
hsSigReplacements _ = mempty


--------------------------------------------------------------------------------
step :: Bool -> String -> Step
step = (makeStep "UnicodeSyntax" .) . step'


--------------------------------------------------------------------------------
step' :: Bool -> String -> Lines -> Module -> Lines
step' alp lg ls modu = Editor.apply edits ls
  where
    edits =
        foldMap hsTyReplacements (everything modu) <>
        foldMap hsSigReplacements (everything modu) <>
        (if alp then addLanguagePragma lg "UnicodeSyntax" modu else mempty)
