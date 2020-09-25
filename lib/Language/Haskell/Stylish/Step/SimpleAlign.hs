--------------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies           #-}
module Language.Haskell.Stylish.Step.SimpleAlign
    ( Config (..)
    , defaultConfig
    , step
    ) where


--------------------------------------------------------------------------------
import           Data.Maybe                      (fromMaybe, maybeToList)
import           Data.List                       (foldl')
import qualified GHC.Hs                          as Hs
import qualified SrcLoc                          as S


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Align
import           Language.Haskell.Stylish.Editor
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Util
import           Language.Haskell.Stylish.Module


--------------------------------------------------------------------------------
data Config = Config
    { cCases            :: !Bool
    , cTopLevelPatterns :: !Bool
    -- TODO: Matches
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
_tlpats :: (S.Located (Hs.HsModule Hs.GhcPs)) -> [[S.Located (Hs.Match Hs.GhcPs (Hs.LHsExpr Hs.GhcPs))]]
_tlpats modu =
  let
    decls      = map S.unLoc (Hs.hsmodDecls (S.unLoc modu))
    binds      = [ bind | Hs.ValD _ bind <- decls ]
    funMatches = map Hs.fun_matches binds
    matches    = map Hs.mg_alts funMatches
  in
    map S.unLoc matches

--------------------------------------------------------------------------------
records :: (S.Located (Hs.HsModule Hs.GhcPs)) -> [[S.Located (Hs.ConDeclField Hs.GhcPs)]]
records modu = do
  let decls           = map S.unLoc (Hs.hsmodDecls (S.unLoc modu))
      tyClDecls       = [ tyClDecl | Hs.TyClD _ tyClDecl <- decls ]
      dataDecls       = [ d | d@(Hs.DataDecl _ _ _ _ _)  <- tyClDecls ]
      dataDefns       = map Hs.tcdDataDefn dataDecls
  d@Hs.ConDeclH98 {} <- concatMap getConDecls dataDefns
  pure $ do
      Hs.RecCon rec <- [Hs.con_args d]
      S.unLoc rec
  {-
      let conDeclDetails  = getConDeclDetails conDecl
          llConDeclFields = getLocRecs conDeclDetails
          lConDeclFields  = concatMap S.unLoc llConDeclFields
    [ lConDeclFields ]
    -}

 where
  getConDecls :: Hs.HsDataDefn Hs.GhcPs -> [Hs.ConDecl Hs.GhcPs]
  getConDecls d@Hs.HsDataDefn {} = map S.unLoc $ Hs.dd_cons d
  getConDecls (Hs.XHsDataDefn x) = Hs.noExtCon x


--------------------------------------------------------------------------------
-- get Arguments from data Construction Declaration
getConDeclDetails :: Hs.ConDecl Hs.GhcPs -> Hs.HsConDeclDetails Hs.GhcPs
getConDeclDetails d@(Hs.ConDeclGADT _ _ _ _ _ _ _ _) = Hs.con_args d
getConDeclDetails d@(Hs.ConDeclH98 _ _ _ _ _ _ _)    = Hs.con_args d
getConDeclDetails (Hs.XConDecl x)                    = Hs.noExtCon x


--------------------------------------------------------------------------------
-- look for Record(s) in a list of Construction Declaration details
getLocRecs :: [Hs.HsConDeclDetails Hs.GhcPs] -> [S.Located [Hs.LConDeclField Hs.GhcPs]]
getLocRecs conDeclDetails =
  [ rec | Hs.RecCon rec <- conDeclDetails ]



_matchToAlignable :: S.Located (Hs.Match Hs.GhcPs (Hs.LHsExpr Hs.GhcPs)) -> Maybe (Alignable S.RealSrcSpan)
_matchToAlignable (S.L matchLoc (Hs.Match _ (Hs.FunRhs name _ _) pats grhss)) = do
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
_matchToAlignable (S.L _ (Hs.Match _ _ [] _)) = Nothing
_matchToAlignable (S.L _ (Hs.Match _ _ _ _ )) = Nothing
_matchToAlignable (S.L _ (Hs.XMatch x))       = Hs.noExtCon x

{-
rhsBodies :: Hs.GRHSs Hs.GhcPs a -> [a]
rhsBodies (Hs.GRHSs _ grhss _) = [body | Hs.GRHS _ _ body <- map S.unLoc grhss]
-}

matchGroupToAlignable
    :: Hs.MatchGroup Hs.GhcPs (Hs.LHsExpr Hs.GhcPs) -> [Alignable S.RealSrcSpan]
matchGroupToAlignable (Hs.XMatchGroup x) = Hs.noExtCon x
matchGroupToAlignable (Hs.MG _ alts _) =
    fromMaybe [] $ traverse caseToAlignable (S.unLoc alts)

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
caseToAlignable (S.L matchLoc (Hs.Match _ (Hs.FunRhs name _ _) pats@(_ : _) grhss)) = do
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
caseToAlignable (S.L _ (Hs.XMatch x))       = Hs.noExtCon x
caseToAlignable (S.L _ (Hs.Match _ _ _ _))  = Nothing

recordToAlignable :: [S.Located (Hs.ConDeclField Hs.GhcPs)] -> [Alignable S.RealSrcSpan]
recordToAlignable = fromMaybe [] . traverse fieldDeclToAlignable

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
step maxColumns config = makeStep "Cases" $ \ls module' ->
    let changes :: ((S.Located (Hs.HsModule Hs.GhcPs)) -> [a]) -> (a -> [Alignable S.RealSrcSpan]) -> [Change String]
        changes search toAlign = concat $
            map (align maxColumns) . map toAlign $ search (parsedModule module')
            {-
            [ change_
            | case_   <- search (parsedModule module')
            , aligns  <- maybeToList (mapM toAlign case_)
            , change_ <- traceOutputtable "aligns" (length aligns) $
                align maxColumns aligns
            ]
            -}
        configured :: [Change String]
        configured = concat $
          -- [changes tlpats matchToAlignable | cTopLevelPatterns config] ++
          [changes records recordToAlignable | cRecords config] ++
          [changes everything matchGroupToAlignable | cCases config]
          -- [changes everything caseToAlignable | cCases config]

        cases = everything (parsedModule module') :: [S.Located (Hs.Match Hs.GhcPs (Hs.LHsExpr Hs.GhcPs))]
        matchgroups = everything (parsedModule module') :: [Hs.MatchGroup Hs.GhcPs (Hs.LHsExpr Hs.GhcPs)]
    in 
    -- traceOutputtable "tlpats" (tlpats $ parsedModule module') $
    traceOutputtable "matchgroups" (length matchgroups) $
    traceOutputtable "records" (records $ parsedModule module') $
    traceOutputtable "cases" (cases) $
    applyChanges configured ls
