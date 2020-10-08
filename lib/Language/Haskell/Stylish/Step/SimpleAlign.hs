--------------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
module Language.Haskell.Stylish.Step.SimpleAlign
    ( Config (..)
    , defaultConfig
    , step
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                   (guard)
import           Data.List                       (foldl', foldl1')
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
    { cCases            :: !Bool
    , cTopLevelPatterns :: !Bool
    , cRecords          :: !Bool
    , cMultiWayIf       :: !Bool
    } deriving (Show)


--------------------------------------------------------------------------------
defaultConfig :: Config
defaultConfig = Config
    { cCases            = True
    , cTopLevelPatterns = True
    , cRecords          = True
    , cMultiWayIf       = True
    }


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
recordToAlignable :: Record -> [Alignable S.RealSrcSpan]
recordToAlignable = fromMaybe [] . traverse fieldDeclToAlignable


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
    -> [Alignable S.RealSrcSpan]
matchGroupToAlignable _conf (Hs.XMatchGroup x) = Hs.noExtCon x
matchGroupToAlignable conf (Hs.MG _ alts _) =
  fromMaybe [] $ traverse (matchToAlignable conf) (S.unLoc alts)


--------------------------------------------------------------------------------
matchToAlignable
    :: Config
    -> S.Located (Hs.Match Hs.GhcPs (Hs.LHsExpr Hs.GhcPs))
    -> Maybe (Alignable S.RealSrcSpan)
matchToAlignable conf (S.L matchLoc m@(Hs.Match _ Hs.CaseAlt pats@(_ : _) grhss)) = do
  let patsLocs   = map S.getLoc pats
      pat        = last patsLocs
      guards     = getGuards m
      guardsLocs = map S.getLoc guards
      left       = foldl' S.combineSrcSpans pat guardsLocs
  guard $ cCases conf
  body     <- rhsBody grhss
  matchPos <- toRealSrcSpan matchLoc
  leftPos  <- toRealSrcSpan left
  rightPos <- toRealSrcSpan $ S.getLoc body
  Just $ Alignable
    { aContainer = matchPos
    , aLeft      = leftPos
    , aRight     = rightPos
    , aRightLead = length "-> "
    }
matchToAlignable conf (S.L matchLoc (Hs.Match _ (Hs.FunRhs name _ _) pats@(_ : _) grhss)) = do
  guard $ cTopLevelPatterns conf
  body <- unguardedRhsBody grhss
  let patsLocs = map S.getLoc pats
      nameLoc  = S.getLoc name
      left     = last (nameLoc : patsLocs)
      bodyLoc  = S.getLoc body
  matchPos <- toRealSrcSpan matchLoc
  leftPos  <- toRealSrcSpan left
  bodyPos  <- toRealSrcSpan bodyLoc
  Just $ Alignable
    { aContainer = matchPos
    , aLeft      = leftPos
    , aRight     = bodyPos
    , aRightLead = length "= "
    }
matchToAlignable _conf (S.L _ (Hs.XMatch x))       = Hs.noExtCon x
matchToAlignable _conf (S.L _ (Hs.Match _ _ _ _))  = Nothing


--------------------------------------------------------------------------------
multiWayIfToAlignable
    :: Hs.LHsExpr Hs.GhcPs
    -> [Alignable S.RealSrcSpan]
multiWayIfToAlignable  (S.L _ (Hs.HsMultiIf _ grhss)) =
  fromMaybe [] $ traverse grhsToAlignable grhss
multiWayIfToAlignable _ = []


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
grhsToAlignable (S.L _ (Hs.XGRHS x))  = Hs.noExtCon x
grhsToAlignable (S.L _ _)             = Nothing


--------------------------------------------------------------------------------
step :: Maybe Int -> Config -> Step
step maxColumns config@(Config {..}) = makeStep "Cases" $ \ls module' ->
    let changes
            :: (S.Located (Hs.HsModule Hs.GhcPs) -> [a])
            -> (a -> [Alignable S.RealSrcSpan])
            -> [Change String]
        changes search toAlign = concat $
            map (align maxColumns) . map toAlign $ search (parsedModule module')

        configured :: [Change String]
        configured = concat $
            [changes records recordToAlignable | cRecords ] ++
            [changes everything (matchGroupToAlignable config)] ++
            [changes everything multiWayIfToAlignable | cMultiWayIf] in
    applyChanges configured ls
