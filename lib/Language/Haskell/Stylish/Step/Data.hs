{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DoAndIfThenElse  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}
module Language.Haskell.Stylish.Step.Data
  ( Config(..)
  , defaultConfig

  , Indent(..)
  , MaxColumns(..)
  , step
  ) where


--------------------------------------------------------------------------------
import           Control.Monad                     (forM_, unless, when)
import           Data.List                         (sortBy)
import           Data.Maybe                        (listToMaybe, maybeToList)
import qualified GHC.Hs                            as GHC
import qualified GHC.Types.Fixity                  as GHC
import qualified GHC.Types.Name.Reader             as GHC
import qualified GHC.Types.SrcLoc                  as GHC
import           Prelude                           hiding (init)


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Comments
import qualified Language.Haskell.Stylish.Editor   as Editor
import           Language.Haskell.Stylish.GHC
import           Language.Haskell.Stylish.Module
import           Language.Haskell.Stylish.Ordering
import           Language.Haskell.Stylish.Printer
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Util


--------------------------------------------------------------------------------
data Indent
    = SameLine
    | Indent !Int
  deriving (Show, Eq)

data MaxColumns
  = MaxColumns !Int
  | NoMaxColumns
  deriving (Show, Eq)

data Config = Config
    { cEquals                  :: !Indent
      -- ^ Indent between type constructor and @=@ sign (measured from column 0)
    , cFirstField              :: !Indent
      -- ^ Indent between data constructor and @{@ line (measured from column with data constructor name)
    , cFieldComment            :: !Int
      -- ^ Indent between column with @{@ and start of field line comment (this line has @cFieldComment = 2@)
    , cDeriving                :: !Int
      -- ^ Indent before @deriving@ lines (measured from column 0)
    , cBreakEnums              :: !Bool
      -- ^ Break enums by newlines and follow the above rules
    , cBreakSingleConstructors :: !Bool
      -- ^ Break single constructors when enabled, e.g. @Indent 2@ will not cause newline after @=@
    , cVia                     :: !Indent
      -- ^ Indentation between @via@ clause and start of deriving column start
    , cCurriedContext          :: !Bool
      -- ^ If true, use curried context. E.g: @allValues :: Enum a => Bounded a => Proxy a -> [a]@
    , cSortDeriving            :: !Bool
      -- ^ If true, will sort type classes in a @deriving@ list.
    , cMaxColumns              :: !MaxColumns
    } deriving (Show)

-- | TODO: pass in MaxColumns?
defaultConfig :: Config
defaultConfig = Config
    { cEquals          = Indent 4
    , cFirstField      = Indent 4
    , cFieldComment    = 2
    , cDeriving        = 4
    , cBreakEnums      = True
    , cBreakSingleConstructors = False
    , cVia             = Indent 4
    , cSortDeriving    = True
    , cMaxColumns      = NoMaxColumns
    , cCurriedContext    = False
    }

step :: Config -> Step
step cfg = makeStep "Data" \ls m -> Editor.apply (changes m) ls
  where
    changes :: Module -> Editor.Edits
    changes = foldMap (formatDataDecl cfg) . dataDecls

    dataDecls :: Module -> [DataDecl]
    dataDecls m = do
        ldecl <- GHC.hsmodDecls $ GHC.unLoc m
        GHC.TyClD _ tycld <- pure $ GHC.unLoc ldecl
        loc <- maybeToList $ GHC.srcSpanToRealSrcSpan $ GHC.getLocA ldecl
        case tycld of
            GHC.DataDecl {..} -> pure $ MkDataDecl
                { dataComments = epAnnComments tcdDExt
                , dataLoc      = loc
                , dataDeclName = tcdLName
                , dataTypeVars = tcdTyVars
                , dataDefn     = tcdDataDefn
                , dataFixity   = tcdFixity
                }
            _ -> []

data DataDecl = MkDataDecl
    { dataComments :: [GHC.LEpaComment]
    , dataLoc      :: GHC.RealSrcSpan
    , dataDeclName :: GHC.LocatedN GHC.RdrName
    , dataTypeVars :: GHC.LHsQTyVars GHC.GhcPs
    , dataDefn     :: GHC.HsDataDefn GHC.GhcPs
    , dataFixity   :: GHC.LexicalFixity
    }


formatDataDecl :: Config -> DataDecl -> Editor.Edits
formatDataDecl cfg@Config{..} decl@MkDataDecl {..} =
    Editor.changeLines originalDeclBlock (const printedDecl)
  where
    originalDeclBlock = Editor.Block
        (GHC.srcSpanStartLine dataLoc)
        (GHC.srcSpanEndLine dataLoc)

    printerConfig = PrinterConfig
        { columns = case cMaxColumns of
            NoMaxColumns -> Nothing
            MaxColumns n -> Just n
        }

    printedDecl = runPrinter_ printerConfig $ putDataDecl cfg decl

putDataDecl :: Config -> DataDecl -> P ()
putDataDecl cfg@Config {..} decl = do
    let defn = dataDefn decl
        constructorComments = commentGroups
            (GHC.srcSpanToRealSrcSpan . GHC.getLocA)
            (GHC.dd_cons defn)
            (dataComments decl)

        onelineEnum =
            isEnum decl && not cBreakEnums &&
            all (not . commentGroupHasComments) constructorComments

    putText $ newOrData decl
    space
    putName decl

    when (isGADT decl) (space >> putText "where")

    when (hasConstructors decl) do
        case (cEquals, cFirstField) of
            (_, Indent x) | isEnum decl && cBreakEnums -> newline >> spaces x
            (_, _)
                | not (isNewtype decl)
                , singleConstructor decl && not cBreakSingleConstructors ->
                    space
            (Indent x, _)
                | onelineEnum -> space
                | otherwise -> newline >> spaces x
            (SameLine, _) -> space

        lineLengthAfterEq <- fmap (+2) getCurrentLineLength

        if  | onelineEnum ->
                putText "=" >> space >> putUnbrokenEnum cfg decl
            | isNewtype decl -> do
                putText "=" >> space
                forM_ (GHC.dd_cons defn) $ putNewtypeConstructor cfg
            | not . null $ GHC.dd_cons defn -> do
                forM_ (flagEnds constructorComments) $ \(CommentGroup {..}, firstGroup, lastGroup) -> do
                    forM_ cgPrior $ \lc -> do
                        putComment $ GHC.unLoc lc
                        consIndent lineLengthAfterEq

                    forM_ (flagEnds cgItems) $ \((lcon, mbInlineComment), firstItem, lastItem) -> do
                        unless (isGADT decl) $ do
                            putText $ if firstGroup && firstItem then "=" else "|"
                            space
                        putConstructor cfg lineLengthAfterEq lcon
                        putMaybeLineComment $ GHC.unLoc <$> mbInlineComment
                        unless (lastGroup && lastItem) $
                            consIndent lineLengthAfterEq

                    forM_ cgFollowing $ \lc -> do
                        consIndent lineLengthAfterEq
                        putComment $ GHC.unLoc lc

            | otherwise ->
                pure ()

    let derivingComments = deepAnnComments (GHC.dd_derivs defn)

    when (hasDeriving decl) do
        if onelineEnum && null derivingComments then
            space
        else do
            forM_ derivingComments $ \lc -> do
                newline
                spaces cDeriving
                putComment $ GHC.unLoc lc
            newline
            spaces cDeriving

        sep (newline >> spaces cDeriving) $ map
            (putDeriving cfg)
            (GHC.dd_derivs defn)
  where
    consIndent eqIndent = newline >> case (cEquals, cFirstField) of
        (SameLine, SameLine) -> spaces (eqIndent - 2)
        (SameLine, Indent y) -> spaces (eqIndent + y - 4)
        (Indent x, Indent _) -> spaces x
        (Indent x, SameLine) -> spaces x

derivingClauseTypes
    :: GHC.HsDerivingClause GHC.GhcPs -> [GHC.LHsSigType GHC.GhcPs]
derivingClauseTypes GHC.HsDerivingClause {..} =
    case GHC.unLoc deriv_clause_tys of
        GHC.DctSingle _ t -> [t]
        GHC.DctMulti _ ts -> ts

putDeriving :: Config -> GHC.LHsDerivingClause GHC.GhcPs -> P ()
putDeriving Config{..} lclause = do
    let clause@GHC.HsDerivingClause {..} = GHC.unLoc lclause
        tys = (if cSortDeriving then sortBy compareOutputableCI else id) $
            map (GHC.sig_body . GHC.unLoc) $
            derivingClauseTypes clause
        headTy = listToMaybe tys
        tailTy = drop 1 tys

    putText "deriving"

    forM_ deriv_clause_strategy $ \lstrat -> case GHC.unLoc lstrat of
        GHC.StockStrategy    {} -> space >> putText "stock"
        GHC.AnyclassStrategy {} -> space >> putText "anyclass"
        GHC.NewtypeStrategy  {} -> space >> putText "newtype"
        GHC.ViaStrategy      {} -> pure ()

    putCond
        withinColumns
        do
            space
            putText "("
            sep
                (comma >> space)
                (fmap putOutputable tys)
            putText ")"
        do
            newline
            spaces indentation
            putText "("

            forM_ headTy \t ->
                space >> putOutputable t

            forM_ tailTy \t -> do
                newline
                spaces indentation
                comma
                space
                putOutputable t

            newline
            spaces indentation
            putText ")"

    forM_ deriv_clause_strategy $ \lstrat -> case GHC.unLoc lstrat of
        GHC.ViaStrategy tp -> do
            case cVia of
                SameLine -> space
                Indent x -> newline >> spaces (x + cDeriving)

            putText "via"
            space
            putType $ case tp of
                GHC.XViaStrategyPs _ ty -> GHC.sig_body $ GHC.unLoc ty
        _ -> pure ()

    -- putEolComment pos
  where
    withinColumns PrinterState{currentLine} =
      case cMaxColumns of
        MaxColumns maxCols -> length currentLine <= maxCols
        NoMaxColumns       -> True

    indentation =
      cDeriving + case cFirstField of
        Indent x -> x
        SameLine -> 0

putUnbrokenEnum :: Config -> DataDecl -> P ()
putUnbrokenEnum cfg decl = sep
    (space >> putText "|" >> space)
    (fmap (putConstructor cfg 0) . GHC.dd_cons . dataDefn $ decl)

putName :: DataDecl -> P ()
putName decl@MkDataDecl{..} =
  if isInfix decl then do
    forM_ firstTvar (\t -> putOutputable t >> space)
    putRdrName dataDeclName
    space
    forM_ secondTvar putOutputable
    maybePutKindSig
  else do
    putRdrName dataDeclName
    forM_ (GHC.hsq_explicit dataTypeVars) (\t -> space >> putOutputable t)
    maybePutKindSig

  where
    firstTvar :: Maybe (GHC.LHsTyVarBndr () GHC.GhcPs)
    firstTvar = listToMaybe $ GHC.hsq_explicit dataTypeVars

    secondTvar :: Maybe (GHC.LHsTyVarBndr () GHC.GhcPs)
    secondTvar = listToMaybe . drop 1 $ GHC.hsq_explicit dataTypeVars

    maybePutKindSig :: Printer ()
    maybePutKindSig = forM_ maybeKindSig (\k -> space >> putText "::" >> space >> putOutputable k)

    maybeKindSig :: Maybe (GHC.LHsKind GHC.GhcPs)
    maybeKindSig = GHC.dd_kindSig dataDefn

putConstructor :: Config -> Int -> GHC.LConDecl GHC.GhcPs -> P ()
putConstructor cfg consIndent lcons = case GHC.unLoc lcons of
  GHC.ConDeclGADT {..} -> do
    -- Put argument to constructor first:
    case con_g_args of
      GHC.PrefixConGADT _ -> sep (comma >> space) $ fmap putRdrName con_names
      GHC.RecConGADT _ -> error . mconcat $
          [ "Language.Haskell.Stylish.Step.Data.putConstructor: "
          , "encountered a GADT with record constructors, not supported yet"
          ]

    -- Put type of constructor:
    space
    putText "::"
    space

    putForAll
        (case GHC.unLoc con_bndrs of
            GHC.HsOuterImplicit {} -> False
            GHC.HsOuterExplicit {} -> True)
        (case GHC.unLoc con_bndrs of
            GHC.HsOuterImplicit {}   -> []
            GHC.HsOuterExplicit {..} -> hso_bndrs)
    forM_ con_mb_cxt $ putContext cfg
    case con_g_args of
        GHC.PrefixConGADT scaledTys -> forM_ scaledTys $ \scaledTy -> do
            putType $ GHC.hsScaledThing scaledTy
            space >> putText "->" >> space
        GHC.RecConGADT _ -> error . mconcat $
            [ "Language.Haskell.Stylish.Step.Data.putConstructor: "
            , "encountered a GADT with record constructors, not supported yet"
            ]
    putType con_res_ty

  GHC.ConDeclH98 {..} -> do
    putForAll con_forall con_ex_tvs
    forM_ con_mb_cxt $ putContext cfg
    case con_args of
      GHC.InfixCon arg1 arg2 -> do
        putType $ GHC.hsScaledThing arg1
        space
        putRdrName con_name
        space
        putType $ GHC.hsScaledThing arg2
      GHC.PrefixCon _tyargs args -> do
        putRdrName con_name
        unless (null args) space
        sep space (fmap putOutputable args)
      GHC.RecCon largs | _ : _ <- GHC.unLoc largs -> do
        putRdrName con_name
        skipToBrace
        bracePos <- getCurrentLineLength
        putText "{"
        let fieldPos = bracePos + 2
        space

        let commented = commentGroups
                (GHC.srcSpanToRealSrcSpan . GHC.getLocA)
                (GHC.unLoc largs)
                (epAnnComments . GHC.ann $ GHC.getLoc largs)

        forM_ (flagEnds commented) $ \(CommentGroup {..}, firstCommentGroup, _) -> do

        -- Unless everything's configured to be on the same line, put pending
        -- comments
          forM_ cgPrior $ \lc -> do
            pad fieldPos
            putComment $ GHC.unLoc lc
            sepDecl bracePos

          forM_ (flagEnds cgItems) $ \((item, mbInlineComment), firstItem, _) -> do
            if firstCommentGroup && firstItem
                then pad fieldPos
                else do
                    comma
                    space
            putConDeclField cfg $ GHC.unLoc item
            case mbInlineComment of
                Just c -> do
                    sepDecl bracePos >> spaces (cFieldComment cfg)
                    putComment $ GHC.unLoc c
                _ -> pure ()
            sepDecl bracePos

          forM_ cgFollowing $ \lc -> do
            spaces $ cFieldComment cfg
            putComment $ GHC.unLoc lc
            sepDecl bracePos

        -- Print whitespace to closing brace
        putText "}"
      GHC.RecCon _ -> do
        skipToBrace >> putText "{"
        skipToBrace >> putText "}"

    where
      -- Jump to the first brace of the first record of the first constructor.
      skipToBrace = case (cEquals cfg, cFirstField cfg) of
        (_, Indent y) | not (cBreakSingleConstructors cfg) -> newline >> spaces y
        (SameLine, SameLine) -> space
        (Indent x, Indent y) -> newline >> spaces (x + y + 2)
        (SameLine, Indent y) -> newline >> spaces (consIndent + y)
        (Indent _, SameLine) -> space

      -- Jump to the next declaration.
      sepDecl bracePos = newline >> spaces case (cEquals cfg, cFirstField cfg) of
        (_, Indent y) | not (cBreakSingleConstructors cfg) -> y
        (SameLine, SameLine)                               -> bracePos
        (Indent x, Indent y)                               -> x + y + 2
        (SameLine, Indent y)                               -> bracePos + y - 2
        (Indent x, SameLine)                               -> bracePos + x - 2

putNewtypeConstructor :: Config -> GHC.LConDecl GHC.GhcPs -> P ()
putNewtypeConstructor cfg lcons = case GHC.unLoc lcons of
  GHC.ConDeclH98{..} ->
    putRdrName con_name >> case con_args of
      GHC.PrefixCon _ args -> do
        unless (null args) space
        sep space (fmap putOutputable args)
      GHC.RecCon largs | [firstArg] <- GHC.unLoc largs -> do
        space
        putText "{"
        space
        putConDeclField cfg $ GHC.unLoc firstArg
        space
        putText "}"
      GHC.RecCon {} ->
        error . mconcat $
          [ "Language.Haskell.Stylish.Step.Data.putNewtypeConstructor: "
          , "encountered newtype with several arguments"
          ]
      GHC.InfixCon {} ->
        error . mconcat $
          [ "Language.Haskell.Stylish.Step.Data.putNewtypeConstructor: "
          , "infix newtype constructor"
          ]
  GHC.ConDeclGADT{} ->
    error . mconcat $
      [ "Language.Haskell.Stylish.Step.Data.putNewtypeConstructor: "
      , "GADT encountered in newtype"
      ]

putForAll
    :: GHC.OutputableBndrFlag s 'GHC.Parsed
    => Bool -> [GHC.LHsTyVarBndr s GHC.GhcPs] -> P ()
putForAll forall ex_tvs = when forall do
    putText "forall"
    space
    sep space $ putOutputable . GHC.unLoc <$> ex_tvs
    dot
    space

putContext :: Config -> GHC.LHsContext GHC.GhcPs -> P ()
putContext Config{..} lctx = suffix (space >> putText "=>" >> space) $
    case ltys of
        [lty] | GHC.HsParTy _ tp <- GHC.unLoc lty, cCurriedContext ->
          putType tp
        [ctx] ->
          putType ctx
        ctxs | cCurriedContext ->
          sep (space >> putText "=>" >> space) (fmap putType ctxs)
        ctxs ->
          parenthesize $ sep (comma >> space) (fmap putType ctxs)
  where
    ltys = GHC.unLoc lctx :: [GHC.LHsType GHC.GhcPs]

putConDeclField :: Config -> GHC.ConDeclField GHC.GhcPs -> P ()
putConDeclField cfg GHC.ConDeclField {..} = do
    sep
        (comma >> space)
        (fmap putOutputable cd_fld_names)
    space
    putText "::"
    space
    putType' cfg cd_fld_type

-- | A variant of 'putType' that takes 'cCurriedContext' into account
putType' :: Config -> GHC.LHsType GHC.GhcPs -> P ()
putType' cfg lty = case GHC.unLoc lty of
    GHC.HsForAllTy GHC.NoExtField tele tp -> do
        putText "forall"
        space
        sep space $ case tele of
            GHC.HsForAllVis   {..} -> putOutputable . GHC.unLoc <$> hsf_vis_bndrs
            GHC.HsForAllInvis {..} -> putOutputable . GHC.unLoc <$> hsf_invis_bndrs
        case tele of
            GHC.HsForAllVis   {} -> space >> putText "->"
            GHC.HsForAllInvis {} -> putText "."
        space
        putType' cfg tp
    GHC.HsQualTy GHC.NoExtField ctx tp -> do
        forM_ ctx $ putContext cfg
        putType' cfg tp
    _ -> putType lty

newOrData :: DataDecl -> String
newOrData decl = if isNewtype decl then "newtype" else "data"

isGADT :: DataDecl -> Bool
isGADT = any isGADTCons . GHC.dd_cons . dataDefn
  where
    isGADTCons c = case GHC.unLoc c of
      GHC.ConDeclGADT {} -> True
      _                  -> False

isNewtype :: DataDecl -> Bool
isNewtype = (== GHC.NewType) . GHC.dd_ND . dataDefn

isInfix :: DataDecl -> Bool
isInfix = (== GHC.Infix) . dataFixity

isEnum :: DataDecl -> Bool
isEnum = all isUnary . GHC.dd_cons . dataDefn
  where
    isUnary c = case GHC.unLoc c of
      GHC.ConDeclH98 {..} -> case con_args of
        GHC.PrefixCon tyargs args -> null tyargs && null args
        _                         -> False
      _ -> False

hasConstructors :: DataDecl -> Bool
hasConstructors = not . null . GHC.dd_cons . dataDefn

singleConstructor :: DataDecl -> Bool
singleConstructor = (== 1) . length . GHC.dd_cons . dataDefn

hasDeriving :: DataDecl -> Bool
hasDeriving = not . null . GHC.dd_derivs . dataDefn
