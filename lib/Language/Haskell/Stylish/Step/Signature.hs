{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TypeApplications #-}
module Language.Haskell.Stylish.Step.Signature where

import           RdrName                          (RdrName)
import           SrcLoc                           (GenLocated (..), Located)
import GHC.Hs.Decls (HsDecl(..))
import GHC.Hs.Binds (Sig(..))
import           GHC.Hs.Extension                 (GhcPs)

--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Block
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Module
import Language.Haskell.Stylish.Editor (change)
import           Language.Haskell.Stylish.GHC (getStartLineUnsafe, getEndLineUnsafe)
import           Language.Haskell.Stylish.Editor (Change, applyChanges)

-- TODO unify with type alias from Data.hs
type ChangeLine = Change String

data Config = Config
  { cMaxColumns :: Int
  }

step :: Config -> Step
step cfg = makeStep "Signature" (\ls m -> applyChanges (changes cfg m) ls)

changes :: Config -> Module -> [ChangeLine]
changes cfg m = fmap (formatSignatureDecl cfg m) (topLevelFunctionSignatures m)

topLevelFunctionSignatures :: Module -> [Located SignatureDecl]
topLevelFunctionSignatures = queryModule @(Located (HsDecl GhcPs)) \case
  L pos (SigD _ (TypeSig _ [name] _)) -> [L pos $ MkSignatureDecl name]
  _ -> []

data SignatureDecl = MkSignatureDecl
  { sigName :: Located RdrName
  }

formatSignatureDecl :: Config -> Module -> Located SignatureDecl -> ChangeLine
formatSignatureDecl _cfg _m ldecl = change originalDeclBlock id
  where
    originalDeclBlock =
      Block (getStartLineUnsafe ldecl) (getEndLineUnsafe ldecl)
