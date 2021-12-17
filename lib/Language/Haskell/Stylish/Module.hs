{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}
module Language.Haskell.Stylish.Module
  ( -- * Data types
    Module
  , Import (..)
  , Comments (..)
  , Lines

    -- * Getters
  , moduleImports
  , moduleImportGroups
  , queryModule
  , groupByLine

    -- * Imports
  , canMergeImport
  , mergeModuleImport
  ) where

--------------------------------------------------------------------------------
import           Data.Function                (on, (&))
import           Data.Generics                (Typeable, everything, mkQ)
import           Data.List                    (nubBy)
import           Data.List.NonEmpty           (NonEmpty (..))

--------------------------------------------------------------------------------
import           GHC.Hs                       (ImportDecl (..),
                                               ImportDeclQualifiedStyle (..))
import qualified GHC.Hs                       as GHC
import           GHC.Hs.Extension             (GhcPs)
import           GHC.Types.SrcLoc             (GenLocated (..))
import           GHC.Types.SrcLoc             (RealSrcSpan (..))
import           GHC.Types.SrcLoc             (Located, unLoc)
import qualified GHC.Types.SrcLoc             as GHC
import           GHC.Utils.Outputable         (Outputable)

--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.GHC

--------------------------------------------------------------------------------
type Lines = [String]


--------------------------------------------------------------------------------
-- | Concrete module type
type Module = GHC.Located GHC.HsModule

-- | Import declaration in module
newtype Import = Import { unImport :: ImportDecl GhcPs }
  deriving newtype (Outputable)

-- | Returns true if the two import declarations can be merged
canMergeImport :: Import -> Import -> Bool
canMergeImport (Import i0) (Import i1) = and $ fmap (\f -> f i0 i1)
  [ (==) `on` unLoc . ideclName
  , (==) `on` ideclPkgQual
  , (==) `on` ideclSource
  , hasMergableQualified `on` ideclQualified
  , (==) `on` ideclImplicit
  , (==) `on` fmap unLoc . ideclAs
  , (==) `on` fmap fst . ideclHiding -- same 'hiding' flags
  ]
  where
    hasMergableQualified QualifiedPre QualifiedPost = True
    hasMergableQualified QualifiedPost QualifiedPre = True
    hasMergableQualified q0 q1                      = q0 == q1

-- | Comments associated with module
newtype Comments = Comments [GHC.RealLocated GHC.EpaComment]

-- | Get module imports
moduleImports :: Module -> [Located Import]
moduleImports (L _ m) =
    GHC.hsmodImports m & fmap \(L pos i) -> L (GHC.locA pos) (Import i)

-- | Get groups of imports from module
moduleImportGroups :: Module -> [NonEmpty (Located Import)]
moduleImportGroups = groupByLine unsafeGetRealSrcSpan . moduleImports

-- The same logic as 'Language.Haskell.Stylish.Module.moduleImportGroups'.
groupByLine :: (a -> RealSrcSpan) -> [a] -> [NonEmpty a]
groupByLine f = go [] Nothing
  where
    go acc _ [] = ne acc
    go acc mbCurrentLine (x:xs) =
      let
        lStart = GHC.srcSpanStartLine (f x)
        lEnd = GHC.srcSpanEndLine (f x) in
      case mbCurrentLine of
        Just lPrevEnd | lPrevEnd + 1 < lStart
          -> ne acc ++ go [x] (Just lEnd) xs
        _ -> go (acc ++ [x]) (Just lEnd) xs

    ne []       = []
    ne (x : xs) = [x :| xs]

-- | Merge two import declarations, keeping positions from the first
--
--   As alluded, this highlights an issue with merging imports. The GHC
--   annotation comments aren't attached to any particular AST node. This
--   means that right now, we're manually reconstructing the attachment. By
--   merging two import declarations, we lose that mapping.
--
--   It's not really a big deal if we consider that people don't usually
--   comment imports themselves. It _is_ however, systemic and it'd be better
--   if we processed comments beforehand and attached them to all AST nodes in
--   our own representation.
mergeModuleImport :: Located Import -> Located Import -> Located Import
mergeModuleImport (L p0 (Import i0)) (L _p1 (Import i1)) =
  L p0 $ Import i0 { ideclHiding = newImportNames }
  where
    newImportNames =
      case (ideclHiding i0, ideclHiding i1) of
        (Just (b, L p imps0), Just (_, L _ imps1)) -> Just (b, L p (imps0 `merge` imps1))
        (Nothing, Nothing) -> Nothing
        (Just x, Nothing) -> Just x
        (Nothing, Just x) -> Just x
    merge xs ys
      = nubBy ((==) `on` showOutputable) (xs ++ ys)

-- | Query the module AST using @f@
queryModule :: Typeable a => (a -> [b]) -> Module -> [b]
queryModule f = everything (++) (mkQ [] f)
