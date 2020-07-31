--------------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies           #-}
module Language.Haskell.Stylish.Step.SimpleAlignGHC
    ( Config (..)
    , defaultConfig
    , step
    ) where


--------------------------------------------------------------------------------
import           Data.Maybe                      (maybeToList)
import           Data.List                       (foldl')
import qualified GHC.Hs                          as Hs
import qualified SrcLoc                          as S


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.AlignGHC
import           Language.Haskell.Stylish.Editor
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Util


--------------------------------------------------------------------------------
data Config = Config
    { cCases            :: !Bool
    , cTopLevelPatterns :: !Bool
    , cRecords          :: !Bool
    } deriving (Show)


--------------------------------------------------------------------------------
defaultConfig :: Config
defaultConfig = Config
    { cCases            = True
    , cTopLevelPatterns = True
    , cRecords          = True
    }


--------------------------------------------------------------------------------
-- 
tlpats :: GHCModule -> [[S.Located (Hs.Match Hs.GhcPs (Hs.LHsExpr Hs.GhcPs))]]
tlpats modu =
  let
    decls      = map S.unLoc (Hs.hsmodDecls (S.unLoc modu))
    binds      = [ bind | Hs.ValD _ bind <- decls ]
    funMatches = map Hs.fun_matches binds
    matches    = map Hs.mg_alts funMatches
  in
    map S.unLoc matches

--------------------------------------------------------------------------------
records :: GHCModule -> [[S.Located (Hs.ConDeclField Hs.GhcPs)]]
records modu = 
  let
    decls           = map S.unLoc (Hs.hsmodDecls (S.unLoc modu))
    tyClDecls       = [ tyClDecl | Hs.TyClD _ tyClDecl <- decls ]
    dataDecls       = [ d | d@(Hs.DataDecl _ _ _ _ _)  <- tyClDecls ]
    dataDefns       = map Hs.tcdDataDefn dataDecls
    conDecls        = concatMap getConDecls dataDefns
    conDeclDetails  = map getConDeclDetails conDecls
    llConDeclFields = getLocRecs conDeclDetails 
    lConDeclFields  = concatMap S.unLoc llConDeclFields    
  in 
    [ lConDeclFields ]


matchToAlignable :: S.Located (Hs.Match Hs.GhcPs (Hs.LHsExpr Hs.GhcPs)) -> Maybe (Alignable S.RealSrcSpan)
matchToAlignable (S.L matchLoc (Hs.Match _ (Hs.FunRhs name _ _) pats grhss)) = do
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
matchToAlignable (S.L _ (Hs.Match _ _ [] _)) = Nothing
matchToAlignable (S.L _ (Hs.Match _ _ _ _ )) = Nothing
matchToAlignable (S.L _ (Hs.XMatch x))       = Hs.noExtCon x

caseToAlignable :: S.Located (Hs.Match Hs.GhcPs (Hs.LHsExpr Hs.GhcPs)) -> Maybe (Alignable S.RealSrcSpan)  
caseToAlignable (S.L matchLoc m@(Hs.Match _ Hs.CaseAlt pats@(_ : _) grhss)) = do
  let patsLocs   = map S.getLoc pats
      pat        = last patsLocs
      guards     = getGuards m
      guardsLocs = map S.getLoc guards
      left       = foldl' S.combineSrcSpans pat guardsLocs
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
caseToAlignable (S.L _ (Hs.XMatch x))       = Hs.noExtCon x
caseToAlignable (S.L _ (Hs.Match _ _ _ _))  = Nothing


fieldDeclToAlignable :: S.Located (Hs.ConDeclField Hs.GhcPs) -> Maybe (Alignable S.RealSrcSpan)
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

step :: Maybe Int -> Config -> Step
step maxColumns config = makeStep "Cases" . Right $ \ls module' ->
    let changes :: (GHCModule -> [[a]]) -> (a -> Maybe (Alignable S.RealSrcSpan)) -> [Change String]
        changes search toAlign =
            [ change_
            | case_   <- search module'
            , aligns  <- maybeToList (mapM toAlign case_)
            , change_ <- align maxColumns aligns
            ]
        configured :: [Change String]
        configured = concat $
          [changes tlpats matchToAlignable | cTopLevelPatterns config] ++
          [changes records fieldDeclToAlignable | cRecords config] ++
          [changes everything caseToAlignable | cCases config]
    in applyChanges configured ls

