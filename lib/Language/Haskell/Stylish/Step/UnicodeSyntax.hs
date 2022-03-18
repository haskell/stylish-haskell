--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Step.UnicodeSyntax
    ( step
    ) where


--------------------------------------------------------------------------------
import qualified GHC.Hs                                        as GHC
import qualified GHC.Types.SrcLoc                              as GHC


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Editor
import           Language.Haskell.Stylish.Module
import qualified Language.Haskell.Stylish.Replacements         as Rpl
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Step.LanguagePragmas (addLanguagePragma)
import           Language.Haskell.Stylish.Util                 (everything)


--------------------------------------------------------------------------------
hsTyReplacements :: GHC.HsType GHC.GhcPs -> Rpl.Replacements
hsTyReplacements (GHC.HsFunTy xann arr _ _)
    | GHC.HsUnrestrictedArrow GHC.NormalSyntax <- arr
    , GHC.AddRarrowAnn (GHC.EpaSpan loc) <- GHC.anns xann =
        Rpl.replaceRealSrcSpan loc "→"
hsTyReplacements (GHC.HsQualTy _ (Just ctx) _)
    | Just arrow <- GHC.ac_darrow . GHC.anns . GHC.ann $ GHC.getLoc ctx
    , (GHC.NormalSyntax, GHC.EpaSpan loc) <- arrow =
        Rpl.replaceRealSrcSpan loc "⇒"
hsTyReplacements _ = mempty


--------------------------------------------------------------------------------
hsSigReplacements :: GHC.Sig GHC.GhcPs -> Rpl.Replacements
hsSigReplacements (GHC.TypeSig ann _ _)
    | GHC.AddEpAnn GHC.AnnDcolon epaLoc <- GHC.asDcolon $ GHC.anns ann
    , GHC.EpaSpan loc <- epaLoc =
        Rpl.replaceRealSrcSpan loc "∷"
hsSigReplacements _ = mempty


--------------------------------------------------------------------------------
step :: Bool -> String -> Step
step = (makeStep "UnicodeSyntax" .) . step'


--------------------------------------------------------------------------------
step' :: Bool -> String -> Lines -> Module -> Lines
step' alp lg ls modu =
    applyChanges
        (if alp then addLanguagePragma lg "UnicodeSyntax" modu else []) $
    Rpl.apply replacement ls
  where
    replacement =
        foldMap hsTyReplacements (everything modu) <>
        foldMap hsSigReplacements (everything modu)
