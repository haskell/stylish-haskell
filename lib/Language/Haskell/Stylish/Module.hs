module Language.Haskell.Stylish.Module
  ( Module
  , ModuleHeader
  , Imports
  , Decls
  , Comments
  , Lines
  , moduleHeader
  , moduleImports
  , makeModule
  , moduleDecls
  , moduleComments

    -- * Internal API getters
  , rawComments
  , rawImports
  , rawModuleName
  , rawModuleExports
  , rawModuleHaddocks
  ) where

--------------------------------------------------------------------------------
import qualified GHC.Hs                          as GHC
import           GHC.Hs.Extension                (GhcPs)
import           GHC.Hs.Decls                    (LHsDecl)
import           GHC.Hs.ImpExp                   (LImportDecl)
import qualified SrcLoc                          as GHC
import qualified Module                          as GHC

--------------------------------------------------------------------------------
type Lines = [String]


--------------------------------------------------------------------------------
-- | Concrete module type
newtype Module = Module { unModule :: GHC.Located (GHC.HsModule GhcPs) }

newtype Decls = Decls [LHsDecl GhcPs]

data Imports = Imports [LImportDecl GhcPs]

data Comments = Comments [GHC.Located String]

data ModuleHeader = ModuleHeader
  { name :: Maybe (GHC.Located GHC.ModuleName)
  , exports :: Maybe (GHC.Located [GHC.LIE GhcPs])
  , haddocks :: Maybe GHC.LHsDocString
  }

makeModule :: GHC.Located (GHC.HsModule GHC.GhcPs) -> Module
makeModule = Module

moduleDecls :: Module -> Decls
moduleDecls = Decls . GHC.hsmodDecls . unLocated . unModule

moduleComments :: Module -> Comments
moduleComments = undefined

moduleImports :: Module -> Imports
moduleImports = Imports . GHC.hsmodImports . unLocated . unModule

moduleHeader :: Module -> ModuleHeader
moduleHeader (Module (GHC.L _ m)) = ModuleHeader
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

rawComments :: Comments -> [GHC.Located String]
rawComments (Comments xs) = xs
