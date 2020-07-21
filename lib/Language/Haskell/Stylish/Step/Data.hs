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
import           Data.List                        (foldl')
import           Data.Maybe                       (listToMaybe, mapMaybe)

--------------------------------------------------------------------------------
import           ApiAnnotation                    (AnnotationComment)
import           BasicTypes                       (LexicalFixity(..))
import           GHC.Hs.Decls                     (LHsDecl, HsDecl(..), HsDataDefn(..))
import           GHC.Hs.Decls                     (TyClDecl(..), NewOrData(..))
import           GHC.Hs.Decls                     (HsDerivingClause(..), DerivStrategy(..))
import           GHC.Hs.Decls                     (ConDecl(..))
import           GHC.Hs.Extension                 (GhcPs)
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
    } deriving (Show)

step :: Config -> Step
step cfg
  = makeStep "Data"
  $ \ls m -> foldl' (formatDataDecl cfg m) ls (dataDecls m)
  where
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

formatDataDecl :: Config -> Module -> Lines -> Located DataDecl -> Lines
formatDataDecl cfg m ls ldecl@(L _pos decl) =
  applyChanges
    [ delete originalDeclBlock
    , insert (getStartLineUnsafe ldecl) printedDecl
    ]
    ls
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
        case cEquals cfg of
          _ | singleConstructor decl && not (cBreakSingleConstructors cfg) ->
            space
          Indent x
            | isEnum decl && not (cBreakEnums cfg) -> space
            | otherwise -> newline >> spaces x
          SameLine -> space

        putText "="
        space

        lineLengthAfterEq <- getCurrentLineLength

        if isEnum decl && not (cBreakEnums cfg) then
          putUnbrokenEnum cfg decl
        else
          sep
            (consIndent lineLengthAfterEq)
            (fmap (putConstructor cfg lineLengthAfterEq) . dd_cons $ defn)

        when (isEnum decl && not (cBreakEnums cfg) && hasDeriving decl) do
          space

        when (isRecord decl && hasDeriving decl) do
          newline
          spaces (cDeriving cfg)

        sep
          (newline >> spaces (cDeriving cfg))
          (fmap putDeriving . unLocated . dd_derivs $ defn)

    consIndent eqIndent = newline >> case (cEquals cfg, cFirstField cfg) of
      (SameLine, SameLine) -> spaces (eqIndent - 2) >> putText "|" >> space
      (SameLine, Indent y) -> spaces (eqIndent + y - 4) >> putText "|" >> space
      (Indent x, Indent _) -> spaces x >> putText "|" >> space
      (Indent x, SameLine) -> spaces x >> putText "|" >> space

data DataDecl = MkDataDecl
  { dataDeclName :: Located RdrName
  , dataTypeVars :: LHsQTyVars GhcPs
  , dataDefn :: HsDataDefn GhcPs
  , dataFixity :: LexicalFixity
  }

putDeriving :: Located (HsDerivingClause GhcPs) -> P ()
putDeriving (L pos clause) = do
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
    L _ (ViaStrategy x) -> do
      space
      putText "via"
      space
      putOutputable x
    _ -> pure ()

  putEolComment pos

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
    forM_ secondTvar (\t -> putOutputable t >> space)
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
  ConDeclGADT{} -> error "Stylish does not support GADTs yet, ConDeclGADT encountered"
  XConDecl{} -> error "XConDecl"
  ConDeclH98{..} ->
    putRdrName con_name >> case con_args of
      InfixCon {} -> error "infix con"
      PrefixCon xs -> do
        unless (null xs) space
        sep space (fmap putOutputable xs)
      RecCon (L recPos (L posFirst firstArg : args)) -> do
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

putConDeclField :: ConDeclField GhcPs -> P ()
putConDeclField XConDeclField{} = pure ()
putConDeclField ConDeclField{..} = do
  sep
    (comma >> space)
    (fmap (putText . showOutputable) cd_fld_names)
  space
  putText "::"
  space
  putOutputable cd_fld_type

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

isRecord :: DataDecl -> Bool
isRecord = any isRecord' . dd_cons . dataDefn
  where
    isRecord' :: Located (ConDecl GhcPs) -> Bool
    isRecord' = \case
      L _ ConDeclH98{con_args = RecCon {}} -> True
      _ -> False

hasConstructors :: DataDecl -> Bool
hasConstructors = not . null . dd_cons . dataDefn

singleConstructor :: DataDecl -> Bool
singleConstructor = (== 1) . length . dd_cons . dataDefn

hasDeriving :: DataDecl -> Bool
hasDeriving = not . null . unLocated . dd_derivs . dataDefn
