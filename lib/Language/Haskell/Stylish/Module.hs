{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
module Language.Haskell.Stylish.Module
  ( -- * Data types
    Module
  , ModuleHeader
  , Imports
  , Decls
  , Comments
  , Lines
  , makeModule

    -- * Getters
  , moduleHeader
  , moduleImports
  , moduleImportGroups
  , moduleDecls
  , moduleComments
  , moduleLanguagePragmas
  , queryModule

    -- * Annotations
  , lookupAnnotation

    -- * Internal API getters
  , rawComments
  , rawImports
  , rawModuleAnnotations
  , rawModuleDecls
  , rawModuleExports
  , rawModuleHaddocks
  , rawModuleName
  ) where

--------------------------------------------------------------------------------
import qualified ApiAnnotation                   as GHC
import           Data.Function                   ((&))
import           Data.Functor                    ((<&>))
import           Data.Generics                   (Typeable, everything, mkQ)
import           Data.Maybe                      (listToMaybe, mapMaybe)
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.List                       (sort)
import           Data.List.NonEmpty              (NonEmpty, nonEmpty)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Lexer                           as GHC
import qualified GHC.Hs                          as GHC
import           GHC.Hs.Extension                (GhcPs)
import           GHC.Hs.Decls                    (LHsDecl)
import           GHC.Hs.ImpExp                   (LImportDecl)
import           SrcLoc                          (GenLocated(..), RealLocated)
import           SrcLoc                          (RealSrcSpan(..), SrcSpan(..))
import           SrcLoc                          (srcSpanStartLine)
import qualified SrcLoc                          as GHC
import qualified Module                          as GHC
import           Util                            (lastMaybe)

--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.GHC

--------------------------------------------------------------------------------
type Lines = [String]


--------------------------------------------------------------------------------
-- | Concrete module type
data Module = Module
  { parsedComments :: [GHC.RealLocated GHC.AnnotationComment]
  , parsedAnnotations :: [(GHC.ApiAnnKey, [GHC.SrcSpan])]
  , parsedAnnotSrcs :: Map RealSrcSpan [GHC.AnnKeywordId]
  , parsedModule :: GHC.Located (GHC.HsModule GhcPs)
  }

-- | Declarations in module
newtype Decls = Decls [LHsDecl GhcPs]

-- | Imports in module
newtype Imports = Imports [LImportDecl GhcPs]

-- | Comments associated with module
newtype Comments = Comments [GHC.RealLocated GHC.AnnotationComment]

-- | A module header is its name, exports and haddock docstring
data ModuleHeader = ModuleHeader
  { name :: Maybe (GHC.Located GHC.ModuleName)
  , exports :: Maybe (GHC.Located [GHC.LIE GhcPs])
  , haddocks :: Maybe GHC.LHsDocString
  }

-- | Create a module from GHC internal representations
makeModule :: GHC.PState -> GHC.Located (GHC.HsModule GHC.GhcPs) -> Module
makeModule pstate = Module comments annotations annotationMap
  where
    comments
      = sort
      . filterRealLocated
      $ GHC.comment_q pstate ++ (GHC.annotations_comments pstate >>= snd)

    filterRealLocated = mapMaybe \case
      GHC.L (GHC.RealSrcSpan s) e -> Just (GHC.L s e)
      GHC.L (GHC.UnhelpfulSpan _) _ -> Nothing

    annotations
      = GHC.annotations pstate

    annotationMap
      = GHC.annotations pstate
      & mapMaybe x
      & Map.fromListWith (++)

    x = \case
      ((RealSrcSpan rspan, annot), _) -> Just (rspan, [annot])
      _ -> Nothing

-- | Get all declarations in module
moduleDecls :: Module -> Decls
moduleDecls = Decls . GHC.hsmodDecls . unLocated . parsedModule

-- | Get comments in module
moduleComments :: Module -> Comments
moduleComments = Comments . parsedComments

-- | Get module language pragmas
moduleLanguagePragmas :: Module -> [(RealSrcSpan, NonEmpty Text)]
moduleLanguagePragmas = mapMaybe toLanguagePragma . parsedComments
  where
    toLanguagePragma :: RealLocated GHC.AnnotationComment -> Maybe (RealSrcSpan, NonEmpty Text)
    toLanguagePragma = \case
      L pos (GHC.AnnBlockComment s) ->
        Just (T.pack s)
          >>= T.stripPrefix "{-#"
          >>= T.stripSuffix "#-}"
          <&> T.strip
          <&> T.splitAt 8 -- length "LANGUAGE" - 1
          <&> fmap (T.splitOn ",")
          <&> fmap (fmap T.strip)
          <&> fmap (filter (not . T.null))
          >>= (\(T.toUpper . T.strip -> lang, xs) -> (lang,) <$> nonEmpty xs)
          >>= (\(lang, nel) -> if lang == "LANGUAGE" then Just (pos, nel) else Nothing)
      _ -> Nothing

-- | Get module imports
moduleImports :: Module -> Imports
moduleImports = Imports . GHC.hsmodImports . unLocated . parsedModule

-- | Get groups of imports from module
moduleImportGroups :: Module -> [Imports]
moduleImportGroups m = go relevantComments imports
  where
    relevantComments
      = moduleComments m
      & rawComments
      & dropBeforeLocated (listToMaybe imports)
      & dropAfterLocated (lastMaybe imports)

    imports = rawImports (moduleImports m)

    go :: [RealLocated GHC.AnnotationComment] -> [LImportDecl GhcPs] -> [Imports]
    go (L nextCommentPos _ : commentsRest) (imp : impRest) =
      let
        sameGroup = takeWhile (\i -> getStartLineUnsafe i < srcSpanStartLine nextCommentPos) impRest
        rest = dropWhile (\i -> getStartLineUnsafe i <= srcSpanStartLine nextCommentPos) impRest
      in
        Imports (imp : sameGroup) : go commentsRest rest
    go _comments imps = [Imports imps]

-- | Get module header
moduleHeader :: Module -> ModuleHeader
moduleHeader (Module _ _ _ (GHC.L _ m)) = ModuleHeader
  { name = GHC.hsmodName m
  , exports = GHC.hsmodExports m
  , haddocks = GHC.hsmodHaddockModHeader m
  }

-- | Query for annotations associated with a 'SrcSpan'
lookupAnnotation :: SrcSpan -> Module -> [GHC.AnnKeywordId]
lookupAnnotation (RealSrcSpan rspan) m = Map.findWithDefault [] rspan (parsedAnnotSrcs m)
lookupAnnotation (UnhelpfulSpan _) _ = []

-- | Query the module AST using @f@
queryModule :: Typeable a => (a -> [b]) -> Module -> [b]
queryModule f = everything (++) (mkQ [] f) . parsedModule

--------------------------------------------------------------------------------
-- | Getter for internal components in imports newtype
rawImports :: Imports -> [LImportDecl GhcPs]
rawImports (Imports xs) = xs

-- | Getter for internal module name representation
rawModuleName :: ModuleHeader -> Maybe (GHC.Located GHC.ModuleName)
rawModuleName = name

-- | Getter for internal module exports representation
rawModuleExports :: ModuleHeader -> Maybe (GHC.Located [GHC.LIE GhcPs])
rawModuleExports = exports

-- | Getter for internal module haddocks representation
rawModuleHaddocks :: ModuleHeader -> Maybe GHC.LHsDocString
rawModuleHaddocks = haddocks

-- | Getter for internal module decls representation
rawModuleDecls :: Decls -> [LHsDecl GhcPs]
rawModuleDecls (Decls xs) = xs

-- | Getter for internal module comments representation
rawComments :: Comments -> [GHC.RealLocated GHC.AnnotationComment]
rawComments (Comments xs) = xs

-- | Getter for internal module annotation representation
rawModuleAnnotations :: Module -> [(GHC.ApiAnnKey, [GHC.SrcSpan])]
rawModuleAnnotations = parsedAnnotations
