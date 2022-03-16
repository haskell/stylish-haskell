{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
-- | Utility functions for working with the GHC AST
module Language.Haskell.Stylish.GHC
  ( dropAfterLocated
  , dropBeforeLocated
  , dropBeforeAndAfter
    -- * Unsafe getters
  , unsafeGetRealSrcSpan
  , getEndLineUnsafe
  , getStartLineUnsafe
    -- * Standard settings
  , baseDynFlags
    -- * Outputable operators
  , showOutputable

    -- * Deconstruction
  , epAnnComments
  , deepAnnComments
  ) where

--------------------------------------------------------------------------------
import           Data.Generics                                       (Data,
                                                                      Typeable,
                                                                      everything,
                                                                      mkQ)
import           Data.List                                           (sortOn)
import qualified GHC.Driver.Ppr                                      as GHC (showPpr)
import           GHC.Driver.Session                                  (defaultDynFlags)
import qualified GHC.Driver.Session                                  as GHC
import qualified GHC.Hs                                              as GHC
import           GHC.Types.SrcLoc                                    (GenLocated (..),
                                                                      Located,
                                                                      RealLocated,
                                                                      RealSrcSpan,
                                                                      SrcSpan (..),
                                                                      srcSpanEndLine,
                                                                      srcSpanStartLine)
import qualified GHC.Types.SrcLoc                                    as GHC
import qualified GHC.Utils.Outputable                                as GHC
import qualified Language.Haskell.GhclibParserEx.GHC.Settings.Config as GHCEx

unsafeGetRealSrcSpan :: Located a -> RealSrcSpan
unsafeGetRealSrcSpan = \case
  (L (RealSrcSpan s _) _) -> s
  _                       -> error "could not get source code location"

getStartLineUnsafe :: Located a -> Int
getStartLineUnsafe = srcSpanStartLine . unsafeGetRealSrcSpan

getEndLineUnsafe :: Located a -> Int
getEndLineUnsafe = srcSpanEndLine . unsafeGetRealSrcSpan

dropAfterLocated :: Maybe (Located a) -> [RealLocated b] -> [RealLocated b]
dropAfterLocated loc xs = case loc of
  Just (L (RealSrcSpan rloc _) _) ->
    filter (\(L x _) -> srcSpanEndLine rloc >= srcSpanStartLine x) xs
  _ -> xs

dropBeforeLocated :: Maybe (Located a) -> [RealLocated b] -> [RealLocated b]
dropBeforeLocated loc xs = case loc of
  Just (L (RealSrcSpan rloc _) _) ->
    filter (\(L x _) -> srcSpanStartLine rloc <= srcSpanEndLine x) xs
  _ -> xs

dropBeforeAndAfter :: Located a -> [RealLocated b] -> [RealLocated b]
dropBeforeAndAfter loc = dropBeforeLocated (Just loc) . dropAfterLocated (Just loc)

baseDynFlags :: GHC.DynFlags
baseDynFlags = defaultDynFlags GHCEx.fakeSettings GHCEx.fakeLlvmConfig

showOutputable :: GHC.Outputable a => a -> String
showOutputable = GHC.showPpr baseDynFlags

epAnnComments :: GHC.EpAnn a -> [GHC.LEpaComment]
epAnnComments GHC.EpAnnNotUsed = []
epAnnComments GHC.EpAnn {..}   = priorAndFollowing comments

deepAnnComments :: (Data a, Typeable a) => a -> [GHC.LEpaComment]
deepAnnComments = everything (++) (mkQ [] priorAndFollowing)

priorAndFollowing :: GHC.EpAnnComments -> [GHC.LEpaComment]
priorAndFollowing = sortOn (GHC.anchor . GHC.getLoc) . \case
    GHC.EpaComments         {..} -> priorComments
    GHC.EpaCommentsBalanced {..} -> priorComments ++ followingComments
