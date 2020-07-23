{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Language.Haskell.Stylish.Step.Data
  ( Config(..)
  , Indent(..)
  , step
  ) where

--------------------------------------------------------------------------------
import           Prelude                          hiding (init)

--------------------------------------------------------------------------------
import           Control.Monad                    (forM_, unless, when)
import           Data.Function                    ((&))
import           Data.Maybe                       (listToMaybe, mapMaybe)

--------------------------------------------------------------------------------
import           ApiAnnotation                    (AnnotationComment)
import           BasicTypes                       (LexicalFixity(..))
import           GHC.Hs.Decls                     (LHsDecl, HsDecl(..), HsDataDefn(..))
import           GHC.Hs.Decls                     (TyClDecl(..), NewOrData(..))
import           GHC.Hs.Decls                     (HsDerivingClause(..), DerivStrategy(..))
import           GHC.Hs.Decls                     (ConDecl(..))
import           GHC.Hs.Extension                 (GhcPs, noExtCon)
import           GHC.Hs.Types                     (ConDeclField(..))
import           GHC.Hs.Types                     (LHsQTyVars(..), HsTyVarBndr(..))
import           GHC.Hs.Types                     (HsConDetails(..), HsImplicitBndrs(..))
import           RdrName                          (RdrName)
import           SrcLoc                           (Located, RealLocated)
import           SrcLoc                           (GenLocated(..))

--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Block
import           Language.Haskell.Stylish.Editor
import           Language.Haskell.Stylish.GHC
import           Language.Haskell.Stylish.Module
import           Language.Haskell.Stylish.Printer
import           Language.Haskell.Stylish.Step

data Indent
    = SameLine
    | Indent !Int
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
    } deriving (Show)

step :: Config -> Step
step cfg = makeStep "Data" \ls m -> applyChanges (changes m) ls
  where
    changes :: Module -> [ChangeLine]
    changes m = fmap (formatDataDecl cfg m) (dataDecls m)

    dataDecls :: Module -> [Located DataDecl]
    dataDecls
      = mapMaybe toDataDecl
      . mapMaybe toTycl
      . rawModuleDecls
      . moduleDecls

    toTycl :: LHsDecl GhcPs -> Maybe (Located (TyClDecl GhcPs))
    toTycl = \case
      L pos (TyClD _ tyClDecl) -> Just (L pos tyClDecl)
      _ -> Nothing

    toDataDecl :: Located (TyClDecl GhcPs) -> Maybe (Located DataDecl)
    toDataDecl = \case
      L pos (DataDecl _ name tvars fixity defn) -> Just . L pos $ MkDataDecl
        { dataDeclName = name
        , dataTypeVars = tvars
        , dataDefn = defn
        , dataFixity = fixity
        }
      _ -> Nothing

type ChangeLine = Change String

formatDataDecl :: Config -> Module -> Located DataDecl -> ChangeLine
formatDataDecl cfg m ldecl@(L declPos decl) =
  change originalDeclBlock (const printedDecl)
  where
    relevantComments :: [RealLocated AnnotationComment]
    relevantComments
      = moduleComments m
      & rawComments
      & dropBeforeAndAfter ldecl

    defn = dataDefn decl

    originalDeclBlock =
      Block (getStartLineUnsafe ldecl) (getEndLineUnsafe ldecl)

    printedDecl = runPrinter_ PrinterConfig relevantComments m do
      putText (newOrData decl)
      space
      putName decl

      when (hasConstructors decl) do
        case (cEquals cfg, cFirstField cfg) of
          (_, Indent x) | isEnum decl && cBreakEnums cfg -> do
            putEolComment declPos
            newline >> spaces x
          (_, _) | not (isNewtype decl) && singleConstructor decl && not (cBreakSingleConstructors cfg) ->
            space
          (Indent x, _)
            | isEnum decl && not (cBreakEnums cfg) -> space
            | otherwise -> do
              putEolComment declPos
              newline >> spaces x
          (SameLine, _) -> space

        lineLengthAfterEq <- fmap (+2) getCurrentLineLength

        if isEnum decl && not (cBreakEnums cfg) then
          putText "=" >> space >> putUnbrokenEnum cfg decl
        else if isNewtype decl then
          putText "=" >> space >> forM_ (dd_cons defn) (putNewtypeConstructor cfg)
        else
          case dd_cons defn of
            [] -> pure ()
            lcon@(L pos _) : consRest -> do
              unless (cFirstField cfg == SameLine || null consRest && not (cBreakSingleConstructors cfg)) do
                removeCommentTo pos >>= mapM_ \c -> putComment c >> consIndent lineLengthAfterEq
              putText "="
              space
              putConstructor cfg lineLengthAfterEq lcon
              forM_ consRest \con@(L conPos _) -> do
                unless (cFirstField cfg == SameLine) do
                  removeCommentTo conPos >>= mapM_ \c -> consIndent lineLengthAfterEq >> putComment c
                consIndent lineLengthAfterEq
                putText "|"
                space
                putConstructor cfg lineLengthAfterEq con
                putEolComment conPos

        when (hasDeriving decl) do
          if isEnum decl && not (cBreakEnums cfg) then
            space
          else do
            newline
            spaces (cDeriving cfg)

        sep
          (newline >> spaces (cDeriving cfg))
          (fmap (putDeriving cfg) . unLocated . dd_derivs $ defn)

    consIndent eqIndent = newline >> case (cEquals cfg, cFirstField cfg) of
      (SameLine, SameLine) -> spaces (eqIndent - 2)
      (SameLine, Indent y) -> spaces (eqIndent + y - 4)
      (Indent x, Indent _) -> spaces x
      (Indent x, SameLine) -> spaces x

data DataDecl = MkDataDecl
  { dataDeclName :: Located RdrName
  , dataTypeVars :: LHsQTyVars GhcPs
  , dataDefn :: HsDataDefn GhcPs
  , dataFixity :: LexicalFixity
  }

putDeriving :: Config -> Located (HsDerivingClause GhcPs) -> P ()
putDeriving cfg (L pos clause) = do
  putText "deriving"
  space

  forM_ (deriv_clause_strategy clause) \case
    L _ StockStrategy -> putText "stock" >> space
    L _ AnyclassStrategy -> putText "anyclass" >> space
    L _ NewtypeStrategy -> putText "newtype" >> space
    L _ (ViaStrategy _) -> pure ()

  putText "("
  sep
    (comma >> space)
    (fmap putOutputable (fmap hsib_body . unLocated . deriv_clause_tys $ clause))
  putText ")"

  forM_ (deriv_clause_strategy clause) \case
    L _ (ViaStrategy tp) -> do
      case cVia cfg of
        SameLine -> space
        Indent x -> newline >> spaces (x + cDeriving cfg)

      putText "via"
      space
      putType (getType tp)
    _ -> pure ()

  putEolComment pos

  where
    getType = \case
      HsIB _ tp -> tp
      XHsImplicitBndrs x -> noExtCon x

putUnbrokenEnum :: Config -> DataDecl -> P ()
putUnbrokenEnum cfg decl =
  sep
    (space >> putText "|" >> space)
    (fmap (putConstructor cfg 0) . dd_cons . dataDefn $ decl)

putName :: DataDecl -> P ()
putName decl@MkDataDecl{..} =
  if isInfix decl then do
    forM_ firstTvar (\t -> putOutputable t >> space)
    putRdrName dataDeclName
    space
    forM_ secondTvar putOutputable
  else do
    putRdrName dataDeclName
    forM_ (hsq_explicit dataTypeVars) (\t -> space >> putOutputable t)

  where
    firstTvar :: Maybe (Located (HsTyVarBndr GhcPs))
    firstTvar
      = dataTypeVars
      & hsq_explicit
      & listToMaybe

    secondTvar :: Maybe (Located (HsTyVarBndr GhcPs))
    secondTvar
      = dataTypeVars
      & hsq_explicit
      & drop 1
      & listToMaybe

putConstructor :: Config -> Int -> Located (ConDecl GhcPs) -> P ()
putConstructor cfg consIndent (L _ cons) = case cons of
  ConDeclGADT{} ->
    error "Stylish does not support GADTs yet, ConDeclGADT encountered"
  XConDecl x ->
    noExtCon x
  ConDeclH98{..} ->
    case con_args of
      InfixCon arg1 arg2 -> do
        putOutputable arg1
        space
        putRdrName con_name
        space
        putOutputable arg2
      PrefixCon xs -> do
        putRdrName con_name
        unless (null xs) space
        sep space (fmap putOutputable xs)
      RecCon (L recPos (L posFirst firstArg : args)) -> do
        putRdrName con_name
        skipToBrace >> putText "{"
        bracePos <- getCurrentLineLength
        space

        -- Unless everything's configured to be on the same line, put pending
        -- comments
        unless (cFirstField cfg == SameLine) do
          removeCommentTo posFirst >>= mapM_ \c -> putComment c >> sepDecl bracePos

        -- Put first decl field
        putConDeclField firstArg
        unless (cFirstField cfg == SameLine) (putEolComment posFirst)

        -- Put tail decl fields
        forM_ args \(L pos arg) -> do
          sepDecl bracePos
          removeCommentTo pos >>= mapM_ \c ->
            spaces (cFieldComment cfg) >> putComment c >> sepDecl bracePos
          comma
          space
          putConDeclField arg
          putEolComment pos

        -- Print docstr after final field
        removeCommentToEnd recPos >>= mapM_ \c ->
          sepDecl bracePos >> spaces (cFieldComment cfg) >> putComment c

        -- Print whitespace to closing brace
        sepDecl bracePos >> putText "}"
      RecCon (L _ []) -> do
        skipToBrace >> putText "{"
        skipToBrace >> putText "}"

    where
      skipToBrace = case (cEquals cfg, cFirstField cfg) of
        (_, Indent y) | not (cBreakSingleConstructors cfg) -> newline >> spaces y
        (SameLine, SameLine) -> space
        (Indent x, Indent y) -> newline >> spaces (x + y + 2)
        (SameLine, Indent y) -> newline >> spaces (consIndent + y)
        (Indent _, SameLine) -> space

      sepDecl bracePos = newline >> spaces case (cEquals cfg, cFirstField cfg) of
        (_, Indent y) | not (cBreakSingleConstructors cfg) -> y
        (SameLine, SameLine) -> bracePos - 1 -- back one from brace pos to place comma
        (Indent x, Indent y) -> x + y + 2
        (SameLine, Indent y) -> bracePos - 1 + y - 2
        (Indent x, SameLine) -> bracePos - 1 + x - 2

putNewtypeConstructor :: Config -> Located (ConDecl GhcPs) -> P ()
putNewtypeConstructor _ (L _ cons) = case cons of
  ConDeclH98{..} ->
    putRdrName con_name >> case con_args of
      PrefixCon xs -> do
        unless (null xs) space
        sep space (fmap putOutputable xs)
      RecCon (L _ [L _posFirst firstArg]) -> do
        space
        putText "{"
        space
        putConDeclField firstArg
        space
        putText "}"
      RecCon (L _ _args) ->
        error . mconcat $
          [ "Language.Haskell.Stylish.Step.Data.putNewtypeConstructor: "
          , "encountered newtype with several arguments"
          ]
      InfixCon {} ->
        error . mconcat $
          [ "Language.Haskell.Stylish.Step.Data.putNewtypeConstructor: "
          , "infix newtype constructor"
          ]
  XConDecl x ->
    noExtCon x
  ConDeclGADT{} ->
    error . mconcat $
      [ "Language.Haskell.Stylish.Step.Data.putNewtypeConstructor: "
      , "GADT encountered in newtype"
      ]

putConDeclField :: ConDeclField GhcPs -> P ()
putConDeclField = \case
  ConDeclField{..} -> do
    sep
      (comma >> space)
      (fmap (putText . showOutputable) cd_fld_names)
    space
    putText "::"
    space
    putType cd_fld_type
  XConDeclField{} ->
    error . mconcat $
      [ "Language.Haskell.Stylish.Step.Data.putConDeclField: "
      , "XConDeclField encountered"
      ]

newOrData :: DataDecl -> String
newOrData decl = if isNewtype decl then "newtype" else "data"

isNewtype :: DataDecl -> Bool
isNewtype = (== NewType) . dd_ND . dataDefn

isInfix :: DataDecl -> Bool
isInfix = (== Infix) . dataFixity

isEnum :: DataDecl -> Bool
isEnum = all isUnary . dd_cons . dataDefn
  where
    isUnary = \case
      L _ (ConDeclH98 {..}) -> case con_args of
        PrefixCon [] -> True
        _ -> False
      _ -> False

hasConstructors :: DataDecl -> Bool
hasConstructors = not . null . dd_cons . dataDefn

singleConstructor :: DataDecl -> Bool
singleConstructor = (== 1) . length . dd_cons . dataDefn

hasDeriving :: DataDecl -> Bool
hasDeriving = not . null . unLocated . dd_derivs . dataDefn
