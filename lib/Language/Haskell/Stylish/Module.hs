{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
module Language.Haskell.Stylish.Module
  ( -- * Data types
    Module
  , Comments (..)
  , Lines

    -- * Getters
  , moduleImportGroups
  , queryModule
  , groupByLine

    -- * Imports
  , canMergeImport
  , mergeModuleImport
  , importModuleName

    -- * Pragmas
  , moduleLanguagePragmas
  ) where


--------------------------------------------------------------------------------
import           Data.Char                    (toLower)
import           Data.Function                (on)
import           Data.Generics                (Typeable, everything, mkQ)
import qualified Data.List                    as L
import           Data.List.NonEmpty           (NonEmpty (..))
import           Data.Maybe                   (fromMaybe, mapMaybe)
import           GHC.Hs                       (ImportDecl (..),
                                               ImportDeclQualifiedStyle (..))
import qualified GHC.Hs                       as GHC
import           GHC.Hs.Extension             (GhcPs)
import           GHC.Types.SrcLoc             (GenLocated (..),
                                               RealSrcSpan (..), unLoc)
import qualified GHC.Types.SrcLoc             as GHC
import qualified GHC.Unit.Module.Name         as GHC


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.GHC


--------------------------------------------------------------------------------
type Lines = [String]


--------------------------------------------------------------------------------
-- | Concrete module type
type Module = GHC.Located GHC.HsModule

importModuleName :: ImportDecl GhcPs -> String
importModuleName = GHC.moduleNameString . GHC.unLoc . GHC.ideclName

-- | Returns true if the two import declarations can be merged
canMergeImport :: ImportDecl GhcPs -> ImportDecl GhcPs -> Bool
canMergeImport i0 i1 = and $ fmap (\f -> f i0 i1)
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

-- | Get groups of imports from module
moduleImportGroups :: Module -> [NonEmpty (GHC.LImportDecl GHC.GhcPs)]
moduleImportGroups =
    groupByLine (fromMaybe err . GHC.srcSpanToRealSrcSpan . GHC.getLocA) .
    GHC.hsmodImports . GHC.unLoc
  where
    err = error "moduleImportGroups: import without soure span"

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
mergeModuleImport
    :: GHC.LImportDecl GHC.GhcPs -> GHC.LImportDecl GHC.GhcPs
    -> GHC.LImportDecl GHC.GhcPs
mergeModuleImport (L p0 i0) (L _p1 i1) =
  L p0 $ i0 { ideclHiding = newImportNames }
  where
    newImportNames =
      case (ideclHiding i0, ideclHiding i1) of
        (Just (b, L p imps0), Just (_, L _ imps1)) -> Just (b, L p (imps0 `merge` imps1))
        (Nothing, Nothing) -> Nothing
        (Just x, Nothing) -> Just x
        (Nothing, Just x) -> Just x
    merge xs ys
      = L.nubBy ((==) `on` showOutputable) (xs ++ ys)

-- | Query the module AST using @f@
queryModule :: Typeable a => (a -> [b]) -> Module -> [b]
queryModule f = everything (++) (mkQ [] f)

moduleLanguagePragmas :: Module -> [(RealSrcSpan, NonEmpty String)]
moduleLanguagePragmas =
    mapMaybe prag . epAnnComments . GHC.hsmodAnn . GHC.unLoc
  where
    prag :: GHC.LEpaComment -> Maybe (GHC.RealSrcSpan, NonEmpty String)
    prag comment = case GHC.ac_tok (GHC.unLoc comment) of
        GHC.EpaBlockComment str
            | lang : p1 : ps <- tokenize str, map toLower lang == "language" ->
                pure (GHC.anchor (GHC.getLoc comment), p1 :| ps)
        _ -> Nothing

    tokenize = words .
        map (\c -> if c == ',' then ' ' else c) .
        takeWhile (/= '#') .
        drop 1 . dropWhile (/= '#')
