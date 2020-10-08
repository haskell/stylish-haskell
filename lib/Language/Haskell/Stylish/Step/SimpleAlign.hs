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
import qualified SrcLoc                          as S


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Align
import           Language.Haskell.Stylish.Editor
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

groupAlign :: Align -> [Alignable S.RealSrcSpan] -> [[Alignable S.RealSrcSpan]]
groupAlign a xs = case a of
    Never    -> []
    Adjacent -> byLine . sortOn (S.srcSpanStartLine . aLeft) $ xs
    Always   -> [xs]
  where
    byLine = map toList . groupByLine aLeft


--------------------------------------------------------------------------------
type Record = [S.Located (Hs.ConDeclField Hs.GhcPs)]


--------------------------------------------------------------------------------
records :: S.Located (Hs.HsModule Hs.GhcPs) -> [Record]
records modu = do
  let decls           = map S.unLoc (Hs.hsmodDecls (S.unLoc modu))
      tyClDecls       = [ tyClDecl | Hs.TyClD _ tyClDecl <- decls ]
      dataDecls       = [ d | d@(Hs.DataDecl _ _ _ _ _)  <- tyClDecls ]
      dataDefns       = map Hs.tcdDataDefn dataDecls
  d@Hs.ConDeclH98 {} <- concatMap getConDecls dataDefns
  case Hs.con_args d of
      Hs.RecCon rec -> [S.unLoc rec]
      _             -> []
 where
  getConDecls :: Hs.HsDataDefn Hs.GhcPs -> [Hs.ConDecl Hs.GhcPs]
  getConDecls d@Hs.HsDataDefn {} = map S.unLoc $ Hs.dd_cons d
  getConDecls (Hs.XHsDataDefn x) = Hs.noExtCon x


--------------------------------------------------------------------------------
recordToAlignable :: Config -> Record -> [[Alignable S.RealSrcSpan]]
recordToAlignable conf = groupAlign (cRecords conf) . fromMaybe [] . traverse fieldDeclToAlignable


--------------------------------------------------------------------------------
fieldDeclToAlignable
    :: S.Located (Hs.ConDeclField Hs.GhcPs) -> Maybe (Alignable S.RealSrcSpan)
fieldDeclToAlignable (S.L _ (Hs.XConDeclField x)) = Hs.noExtCon x
fieldDeclToAlignable (S.L matchLoc (Hs.ConDeclField _ names ty _)) = do
  matchPos <- toRealSrcSpan matchLoc
  leftPos  <- toRealSrcSpan $ S.getLoc $ last names
  tyPos    <- toRealSrcSpan $ S.getLoc ty
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
    -> [[Alignable S.RealSrcSpan]]
matchGroupToAlignable _conf (Hs.XMatchGroup x) = Hs.noExtCon x
matchGroupToAlignable conf (Hs.MG _ alts _) = cases' ++ patterns'
  where
    (cases, patterns) = partitionEithers . fromMaybe [] $ traverse matchToAlignable (S.unLoc alts)
    cases' = groupAlign (cCases conf) cases
    patterns' = groupAlign (cTopLevelPatterns conf) patterns


--------------------------------------------------------------------------------
matchToAlignable
    :: S.Located (Hs.Match Hs.GhcPs (Hs.LHsExpr Hs.GhcPs))
    -> Maybe (Either (Alignable S.RealSrcSpan) (Alignable S.RealSrcSpan))
matchToAlignable (S.L matchLoc m@(Hs.Match _ Hs.CaseAlt pats@(_ : _) grhss)) = do
  let patsLocs   = map S.getLoc pats
      pat        = last patsLocs
      guards     = getGuards m
      guardsLocs = map S.getLoc guards
      left       = foldl' S.combineSrcSpans pat guardsLocs
  body     <- rhsBody grhss
  matchPos <- toRealSrcSpan matchLoc
  leftPos  <- toRealSrcSpan left
  rightPos <- toRealSrcSpan $ S.getLoc body
  Just . Left $ Alignable
    { aContainer = matchPos
    , aLeft      = leftPos
    , aRight     = rightPos
    , aRightLead = length "-> "
    }
matchToAlignable (S.L matchLoc (Hs.Match _ (Hs.FunRhs name _ _) pats@(_ : _) grhss)) = do
  body <- unguardedRhsBody grhss
  let patsLocs = map S.getLoc pats
      nameLoc  = S.getLoc name
      left     = last (nameLoc : patsLocs)
      bodyLoc  = S.getLoc body
  matchPos <- toRealSrcSpan matchLoc
  leftPos  <- toRealSrcSpan left
  bodyPos  <- toRealSrcSpan bodyLoc
  Just . Right $ Alignable
    { aContainer = matchPos
    , aLeft      = leftPos
    , aRight     = bodyPos
    , aRightLead = length "= "
    }
matchToAlignable (S.L _ (Hs.XMatch x))      = Hs.noExtCon x
matchToAlignable (S.L _ (Hs.Match _ _ _ _)) = Nothing


--------------------------------------------------------------------------------
multiWayIfToAlignable
    :: Config
    -> Hs.LHsExpr Hs.GhcPs
    -> [[Alignable S.RealSrcSpan]]
multiWayIfToAlignable conf (S.L _ (Hs.HsMultiIf _ grhss)) =
    groupAlign (cMultiWayIf conf) as
  where
    as = fromMaybe [] $ traverse grhsToAlignable grhss
multiWayIfToAlignable _conf _ = []


--------------------------------------------------------------------------------
grhsToAlignable
    :: S.Located (Hs.GRHS Hs.GhcPs (Hs.LHsExpr Hs.GhcPs))
    -> Maybe (Alignable S.RealSrcSpan)
grhsToAlignable (S.L grhsloc (Hs.GRHS _ guards@(_ : _) body)) = do
    let guardsLocs = map S.getLoc guards
        bodyLoc    = S.getLoc body
        left       = foldl1' S.combineSrcSpans guardsLocs
    matchPos <- toRealSrcSpan grhsloc
    leftPos  <- toRealSrcSpan left
    bodyPos  <- toRealSrcSpan bodyLoc
    Just $ Alignable
        { aContainer = matchPos
        , aLeft      = leftPos
        , aRight     = bodyPos
        , aRightLead = length "-> "
        }
grhsToAlignable (S.L _ (Hs.XGRHS x)) = Hs.noExtCon x
grhsToAlignable (S.L _ _)            = Nothing


--------------------------------------------------------------------------------
step :: Maybe Int -> Config -> Step
step maxColumns config@(Config {..}) = makeStep "Cases" $ \ls module' ->
    let changes
            :: (S.Located (Hs.HsModule Hs.GhcPs) -> [a])
            -> (a -> [[Alignable S.RealSrcSpan]])
            -> [Change String]
        changes search toAlign =
            (concatMap . concatMap) (align maxColumns) . map toAlign $ search (parsedModule module')

        configured :: [Change String]
        configured = concat $
            [changes records (recordToAlignable config)] ++
            [changes everything (matchGroupToAlignable config)] ++
            [changes everything (multiWayIfToAlignable config)] in
    applyChanges configured ls
