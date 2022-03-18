--------------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
module Language.Haskell.Stylish.Step.SimpleAlign
    ( Config (..)
    , Align (..)
    , defaultConfig
    , step
    ) where


--------------------------------------------------------------------------------
import           Data.Either                     (partitionEithers)
import           Data.Foldable                   (toList)
import           Data.List                       (foldl', foldl1', sortOn)
import           Data.Maybe                      (fromMaybe)
import qualified GHC.Hs                          as Hs
import qualified GHC.Parser.Annotation as GHC
import qualified GHC.Types.SrcLoc as GHC


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Align
import          qualified Language.Haskell.Stylish.Editor as Editor
import           Language.Haskell.Stylish.Module
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Util


--------------------------------------------------------------------------------
data Config = Config
    { cCases            :: Align
    , cTopLevelPatterns :: Align
    , cRecords          :: Align
    , cMultiWayIf       :: Align
    } deriving (Show)

data Align
    = Always
    | Adjacent
    | Never
    deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config
    { cCases            = Always
    , cTopLevelPatterns = Always
    , cRecords          = Always
    , cMultiWayIf       = Always
    }

groupAlign :: Align -> [Alignable GHC.RealSrcSpan] -> [[Alignable GHC.RealSrcSpan]]
groupAlign a xs = case a of
    Never    -> []
    Adjacent -> byLine . sortOn (GHC.srcSpanStartLine . aLeft) $ xs
    Always   -> [xs]
  where
    byLine = map toList . groupByLine aLeft


--------------------------------------------------------------------------------
type Record = [GHC.LocatedA (Hs.ConDeclField Hs.GhcPs)]


--------------------------------------------------------------------------------
records :: GHC.Located Hs.HsModule -> [Record]
records modu = do
  let decls           = map GHC.unLoc (Hs.hsmodDecls (GHC.unLoc modu))
      tyClDecls       = [ tyClDecl | Hs.TyClD _ tyClDecl <- decls ]
      dataDecls       = [ d | d@(Hs.DataDecl _ _ _ _ _)  <- tyClDecls ]
      dataDefns       = map Hs.tcdDataDefn dataDecls
  d@Hs.ConDeclH98 {} <- concatMap getConDecls dataDefns
  case Hs.con_args d of
      Hs.RecCon rec -> [GHC.unLoc rec]
      _             -> []
 where
  getConDecls :: Hs.HsDataDefn Hs.GhcPs -> [Hs.ConDecl Hs.GhcPs]
  getConDecls d@Hs.HsDataDefn {} = map GHC.unLoc $ Hs.dd_cons d


--------------------------------------------------------------------------------
recordToAlignable :: Config -> Record -> [[Alignable GHC.RealSrcSpan]]
recordToAlignable conf = groupAlign (cRecords conf) . fromMaybe [] . traverse fieldDeclToAlignable


--------------------------------------------------------------------------------
fieldDeclToAlignable
    :: GHC.LocatedA (Hs.ConDeclField Hs.GhcPs) -> Maybe (Alignable GHC.RealSrcSpan)
fieldDeclToAlignable (GHC.L matchLoc (Hs.ConDeclField _ names ty _)) = do
  matchPos <- GHC.srcSpanToRealSrcSpan $ GHC.locA matchLoc
  leftPos  <- GHC.srcSpanToRealSrcSpan $ GHC.getLoc $ last names
  tyPos    <- GHC.srcSpanToRealSrcSpan $ GHC.getLocA ty
  Just $ Alignable
    { aContainer = matchPos
    , aLeft      = leftPos
    , aRight     = tyPos
    , aRightLead = length ":: "
    }


--------------------------------------------------------------------------------
matchGroupToAlignable
    :: Config
    -> Hs.MatchGroup Hs.GhcPs (Hs.LHsExpr Hs.GhcPs)
    -> [[Alignable GHC.RealSrcSpan]]
matchGroupToAlignable conf (Hs.MG _ alts _) = cases' ++ patterns'
  where
    (cases, patterns) = partitionEithers . fromMaybe [] $ traverse matchToAlignable (GHC.unLoc alts)
    cases' = groupAlign (cCases conf) cases
    patterns' = groupAlign (cTopLevelPatterns conf) patterns


--------------------------------------------------------------------------------
matchToAlignable
    :: GHC.LocatedA (Hs.Match Hs.GhcPs (Hs.LHsExpr Hs.GhcPs))
    -> Maybe (Either (Alignable GHC.RealSrcSpan) (Alignable GHC.RealSrcSpan))
matchToAlignable (GHC.L matchLoc m@(Hs.Match _ Hs.CaseAlt pats@(_ : _) grhss)) = do
  let patsLocs   = map GHC.getLocA pats
      pat        = last patsLocs
      guards     = getGuards m
      guardsLocs = map GHC.getLocA guards
      left       = foldl' GHC.combineSrcSpans pat guardsLocs
  body     <- rhsBody grhss
  matchPos <- GHC.srcSpanToRealSrcSpan $ GHC.locA matchLoc
  leftPos  <- GHC.srcSpanToRealSrcSpan left
  rightPos <- GHC.srcSpanToRealSrcSpan $ GHC.getLocA body
  Just . Left $ Alignable
    { aContainer = matchPos
    , aLeft      = leftPos
    , aRight     = rightPos
    , aRightLead = length "-> "
    }
matchToAlignable (GHC.L matchLoc (Hs.Match _ (Hs.FunRhs name _ _) pats@(_ : _) grhss)) = do
  body <- unguardedRhsBody grhss
  let patsLocs = map GHC.getLocA pats
      nameLoc  = GHC.getLocA name
      left     = last (nameLoc : patsLocs)
      bodyLoc  = GHC.getLocA body
  matchPos <- GHC.srcSpanToRealSrcSpan $ GHC.locA matchLoc
  leftPos  <- GHC.srcSpanToRealSrcSpan left
  bodyPos  <- GHC.srcSpanToRealSrcSpan bodyLoc
  Just . Right $ Alignable
    { aContainer = matchPos
    , aLeft      = leftPos
    , aRight     = bodyPos
    , aRightLead = length "= "
    }
matchToAlignable (GHC.L _ (Hs.Match _ _ _ _)) = Nothing


--------------------------------------------------------------------------------
multiWayIfToAlignable
    :: Config
    -> Hs.LHsExpr Hs.GhcPs
    -> [[Alignable GHC.RealSrcSpan]]
multiWayIfToAlignable conf (GHC.L _ (Hs.HsMultiIf _ grhss)) =
    groupAlign (cMultiWayIf conf) as
  where
    as = fromMaybe [] $ traverse grhsToAlignable grhss
multiWayIfToAlignable _conf _ = []


--------------------------------------------------------------------------------
grhsToAlignable
    :: GHC.Located (Hs.GRHS Hs.GhcPs (Hs.LHsExpr Hs.GhcPs))
    -> Maybe (Alignable GHC.RealSrcSpan)
grhsToAlignable (GHC.L grhsloc (Hs.GRHS _ guards@(_ : _) body)) = do
    let guardsLocs = map GHC.getLocA guards
        bodyLoc    = GHC.getLocA $ body
        left       = foldl1' GHC.combineSrcSpans guardsLocs
    matchPos <- GHC.srcSpanToRealSrcSpan grhsloc
    leftPos  <- GHC.srcSpanToRealSrcSpan left
    bodyPos  <- GHC.srcSpanToRealSrcSpan bodyLoc
    Just $ Alignable
        { aContainer = matchPos
        , aLeft      = leftPos
        , aRight     = bodyPos
        , aRightLead = length "-> "
        }
grhsToAlignable (GHC.L _ _)            = Nothing


--------------------------------------------------------------------------------
step :: Maybe Int -> Config -> Step
step maxColumns config = makeStep "Cases" $ \ls module' ->
    let changes
            :: (GHC.Located Hs.HsModule -> [a])
            -> (a -> [[Alignable GHC.RealSrcSpan]])
            -> Editor.Edits
        changes search toAlign = mconcat $ do
            item <- search module'
            pure $ foldMap (align maxColumns) (toAlign item)

        configured :: Editor.Edits
        configured =
            changes records (recordToAlignable config) <>
            changes everything (matchGroupToAlignable config) <>
            changes everything (multiWayIfToAlignable config) in
    Editor.apply configured ls
