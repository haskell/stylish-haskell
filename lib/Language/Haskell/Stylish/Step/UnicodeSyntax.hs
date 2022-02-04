--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Step.UnicodeSyntax
    ( step
    ) where


--------------------------------------------------------------------------------
import qualified Data.Map              as M
import           Data.Maybe            (mapMaybe)
import           Debug.Trace
import qualified GHC.Hs                as GHC
import qualified GHC.Types.SrcLoc      as GHC


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Module
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Util   (everything)


{-
--------------------------------------------------------------------------------
unicodeReplacements :: Map String String
unicodeReplacements = M.fromList
    [ ("::", "∷")
    , ("=>", "⇒")
    , ("->", "→")
    , ("<-", "←")
    , ("forall", "∀")
    , ("-<", "↢")
    , (">-", "↣")
    ]
-}

--------------------------------------------------------------------------------
-- Simple type that can do replacments on single lines (not spanning, removing
-- or adding lines).
newtype Replacement = Replacement
    { unReplacement :: M.Map Int [(Int, Int, String)]
    } deriving (Show)


--------------------------------------------------------------------------------
instance Semigroup Replacement where
    Replacement l <> Replacement r = Replacement $ M.unionWith (++) l r


--------------------------------------------------------------------------------
instance Monoid Replacement where
    mempty = Replacement mempty


--------------------------------------------------------------------------------
mkReplacement :: GHC.RealSrcSpan -> String -> Replacement
mkReplacement rss repl
    | GHC.srcSpanStartLine rss /= GHC.srcSpanEndLine rss = Replacement mempty
    | otherwise                                          = Replacement $
        M.singleton
            (GHC.srcSpanStartLine rss)
            [(GHC.srcSpanStartCol rss, GHC.srcSpanEndCol rss, repl)]


--------------------------------------------------------------------------------
applyReplacement :: Replacement -> [String] -> [String]
applyReplacement (Replacement repl) ls = do
    (i, l) <- zip [1 ..] ls
    case M.lookup i repl of
        Nothing    -> pure l
        Just repls -> pure $ go repls l
  where
    go [] l = l
    go ((xstart, xend, x) : repls) l =
        let l' = take (xstart - 1) l ++ x ++ drop (xend - 1) l in
        go (mapMaybe (adjust (xstart, xend, x)) repls) l'

    adjust (xstart, xend, x) (ystart, yend, y)
        | yend < xstart  = Just (ystart, yend, y)
        | ystart > xend =
            let offset = length x - (xend - xstart + 1) in
            Just (ystart + offset, yend + offset, y)
        | otherwise     = Nothing


--------------------------------------------------------------------------------
hsTyReplacements :: GHC.HsType GHC.GhcPs -> Replacement
hsTyReplacements (GHC.HsFunTy xann arr _ _)
    | GHC.HsUnrestrictedArrow GHC.NormalSyntax <- arr
    , GHC.AddRarrowAnn (GHC.EpaSpan loc) <- GHC.anns xann =
        mkReplacement loc "→"
hsTyReplacements (GHC.HsQualTy _ (Just ctx) _)
    | Just arrow <- GHC.ac_darrow . GHC.anns . GHC.ann $ GHC.getLoc ctx
    , (GHC.NormalSyntax, GHC.EpaSpan loc) <- arrow =
        mkReplacement loc "⇒"
hsTyReplacements _ = mempty


--------------------------------------------------------------------------------
hsSigReplacements :: GHC.Sig GHC.GhcPs -> Replacement
hsSigReplacements (GHC.TypeSig ann _ _)
    | GHC.AddEpAnn GHC.AnnDcolon epaLoc <- GHC.asDcolon $ GHC.anns ann
    , GHC.EpaSpan loc <- epaLoc =
        mkReplacement loc "∷"
hsSigReplacements _ = mempty


--------------------------------------------------------------------------------
step :: Bool -> String -> Step
step = (makeStep "UnicodeSyntax" .) . step'


--------------------------------------------------------------------------------
step' :: Bool -> String -> Lines -> Module -> Lines
step' _alp _lg ls modu =
    traceShow replacement $
    applyReplacement replacement ls
  where
    replacement =
        foldMap hsTyReplacements (everything modu) <>
        foldMap hsSigReplacements (everything modu)

{-
step' alp lg ls module' = applyChanges changes ls
  where
    changes = (if alp then addLanguagePragma lg "UnicodeSyntax" module' else []) ++
        replaceAll perLine
    toReplace = [ "::", "=>", "->" ]
    perLine = sort $ groupPerLine $ concatMap (findSymbol module' ls) toReplace

-}
