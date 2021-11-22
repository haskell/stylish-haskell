--------------------------------------------------------------------------------
-- | There are a number of steps that sort items: 'Imports' and 'ModuleHeader',
-- and maybe more in the future.  This module provides consistent sorting
-- utilities.
{-# LANGUAGE LambdaCase #-}
module Language.Haskell.Stylish.Ordering
    ( compareImports
    , compareLIE
    , compareWrappedName
    , compareOutputableCI
    , unwrapName
    ) where


--------------------------------------------------------------------------------
import           Data.Char                       (isUpper, toLower)
import           Data.Function                   (on)
import           Data.Ord                        (comparing)
import           GHC.Hs
import           Language.Haskell.Stylish.GHC    (showOutputable)
import           Language.Haskell.Stylish.Module (Import (..))
import           Outputable                      (Outputable)
import qualified Outputable                      as GHC
import           RdrName                         (RdrName)
import           SrcLoc                          (unLoc)



--------------------------------------------------------------------------------
-- | Compare imports for sorting.  Cannot easily be a lawful instance due to
-- case insensitivity.
compareImports :: Import -> Import -> Ordering
compareImports (Import i0) (Import i1) =
    ideclName i0 `compareOutputableCI` ideclName i1 <>
    fmap showOutputable (ideclPkgQual i0) `compare`
        fmap showOutputable (ideclPkgQual i1) <>
    compareOutputableCI i0 i1


--------------------------------------------------------------------------------
-- | NOTE: Can we get rid off this by adding a properly sorting newtype around
-- 'RdrName'?
compareLIE :: LIE GhcPs -> LIE GhcPs -> Ordering
compareLIE = comparing $ ieKey . unLoc
  where
    -- | The implementation is a bit hacky to get proper sorting for input specs:
    -- constructors first, followed by functions, and then operators.
    ieKey :: IE GhcPs -> (Int, String)
    ieKey = \case
        IEVar _ n             -> nameKey n
        IEThingAbs _ n        -> nameKey n
        IEThingAll _ n        -> nameKey n
        IEThingWith _ n _ _ _ -> nameKey n
        IEModuleContents _ n  -> nameKey n
        _                     -> (2, "")


--------------------------------------------------------------------------------
compareWrappedName :: IEWrappedName RdrName -> IEWrappedName RdrName -> Ordering
compareWrappedName = comparing nameKey


--------------------------------------------------------------------------------
unwrapName :: IEWrappedName n -> n
unwrapName (IEName n)    = unLoc n
unwrapName (IEPattern n) = unLoc n
unwrapName (IEType n)    = unLoc n


--------------------------------------------------------------------------------
nameKey :: Outputable name => name -> (Int, String)
nameKey n = case showOutputable n of
    o@('(' : _)             -> (2, o)
    o@(o0 : _) | isUpper o0 -> (0, o)
    o                       -> (1, o)


--------------------------------------------------------------------------------
compareOutputableCI :: GHC.Outputable a => a -> a -> Ordering
compareOutputableCI = compare `on` (map toLower . showOutputable)
