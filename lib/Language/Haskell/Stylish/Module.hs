{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
module Language.Haskell.Stylish.Module
  ( Module
  , ModuleHeader
  , Imports
  , Decls
  , Comments
  , Lines
  , moduleHeader
  , moduleImports
  , moduleImportGroups
  , makeModule
  , moduleDecls
  , moduleComments

    -- * Internal API getters
  , rawComments
  , rawImports
  , rawModuleAnnotations
  , rawModuleExports
  , rawModuleHaddocks
  , rawModuleName
  ) where

--------------------------------------------------------------------------------
import qualified ApiAnnotation                   as GHC
import           Data.Function                   ((&))
import           Data.Maybe                      (listToMaybe, mapMaybe)
import           Data.List                       (sort)
import qualified Lexer                           as GHC
import qualified GHC.Hs                          as GHC
import           GHC.Hs.Extension                (GhcPs)
import           GHC.Hs.Decls                    (LHsDecl)
import           GHC.Hs.ImpExp                   (LImportDecl)
import           SrcLoc                          (GenLocated(..), RealLocated)
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
  , parsedModule :: GHC.Located (GHC.HsModule GhcPs)
  }

newtype Decls = Decls [LHsDecl GhcPs]

data Imports = Imports [LImportDecl GhcPs]

data Comments = Comments [GHC.RealLocated GHC.AnnotationComment]

data ModuleHeader = ModuleHeader
  { name :: Maybe (GHC.Located GHC.ModuleName)
  , exports :: Maybe (GHC.Located [GHC.LIE GhcPs])
  , haddocks :: Maybe GHC.LHsDocString
  }

makeModule :: GHC.PState -> GHC.Located (GHC.HsModule GHC.GhcPs) -> Module
makeModule pstate = Module comments annotations
  where
    comments
      = sort
      . filterRealLocated
      $ GHC.comment_q pstate ++ (GHC.annotations_comments pstate >>= snd)

    filterRealLocated = mapMaybe \case
      GHC.L (GHC.RealSrcSpan s) e -> Just (GHC.L s e)
      GHC.L (GHC.UnhelpfulSpan _) _ -> Nothing

    annotations = GHC.annotations pstate

moduleDecls :: Module -> Decls
moduleDecls = Decls . GHC.hsmodDecls . unLocated . parsedModule

moduleComments :: Module -> Comments
moduleComments = Comments . parsedComments

moduleImports :: Module -> Imports
moduleImports = Imports . GHC.hsmodImports . unLocated . parsedModule

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

moduleHeader :: Module -> ModuleHeader
moduleHeader (Module _ _ (GHC.L _ m)) = ModuleHeader
  { name = GHC.hsmodName m
  , exports = GHC.hsmodExports m
  , haddocks = GHC.hsmodHaddockModHeader m
  }

unLocated :: GHC.Located a -> a
unLocated (GHC.L _ a) = a

--------------------------------------------------------------------------------
-- | Getter for internal components in imports newtype
--
--   /Note:/ this function might be
rawImports :: Imports -> [LImportDecl GhcPs]
rawImports (Imports xs) = xs

rawModuleName :: ModuleHeader -> Maybe (GHC.Located GHC.ModuleName)
rawModuleName = name

rawModuleExports :: ModuleHeader -> Maybe (GHC.Located [GHC.LIE GhcPs])
rawModuleExports = exports

rawModuleHaddocks :: ModuleHeader -> Maybe GHC.LHsDocString
rawModuleHaddocks = haddocks

rawComments :: Comments -> [GHC.RealLocated GHC.AnnotationComment]
rawComments (Comments xs) = xs

rawModuleAnnotations :: Module -> [(GHC.ApiAnnKey, [GHC.SrcSpan])]
rawModuleAnnotations = parsedAnnotations
