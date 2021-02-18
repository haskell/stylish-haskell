{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
module Language.Haskell.Stylish.Step.Signature where

import           RdrName                          (RdrName)
import           SrcLoc                           (GenLocated (..), Located)
import GHC.Hs.Decls
import GHC.Hs.Binds
import GHC.Hs.Types
import           GHC.Hs.Extension                 (GhcPs)

--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Block
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Module
import Language.Haskell.Stylish.Editor (change, noop)
import           Language.Haskell.Stylish.GHC (getStartLineUnsafe, getEndLineUnsafe, getEndColumnUnsafe)
import           Language.Haskell.Stylish.Editor (Change, applyChanges)
import           Language.Haskell.Stylish.Printer

-- TODO unify with type alias from Data.hs
type ChangeLine = Change String

data MaxColumns
  = MaxColumns !Int
  | NoMaxColumns
  deriving (Show, Eq)

fits :: Int -> MaxColumns -> Bool
fits _ NoMaxColumns = True
fits v (MaxColumns limit) = v <= limit

data Config = Config
  { cMaxColumns :: MaxColumns
  }

step :: Config -> Step
step cfg = makeStep "Signature" (\ls m -> applyChanges (changes cfg m) ls)

changes :: Config -> Module -> [ChangeLine]
changes cfg m = fmap (formatSignatureDecl cfg m) (topLevelFunctionSignatures m)

topLevelFunctionSignatures :: Module -> [Located SignatureDecl]
topLevelFunctionSignatures = queryModule @(Located (HsDecl GhcPs)) \case
  L pos (SigD _ (TypeSig _ [name] (HsWC _ (HsIB _ (L _ funTy@(HsFunTy _ _ _ )))))) ->
    [L pos $ MkSignatureDecl name (listParameters funTy)]
  _ -> []

listParameters :: HsType GhcPs -> [Located RdrName]
listParameters (HsFunTy _ (L _ arg2) (L _ arg3)) = listParameters arg2 <> listParameters arg3
listParameters (HsTyVar _ _promotionFlag name) = [name]
listParameters _ = []

data SignatureDecl = MkSignatureDecl
  { sigName :: Located RdrName
  , sigParameters :: [Located RdrName]
  }

formatSignatureDecl :: Config -> Module -> Located SignatureDecl -> ChangeLine
formatSignatureDecl Config{..} m ldecl@(L _declPos decl) = do
  let block = originalDeclBlock
      declLength = getEndColumnUnsafe ldecl
  if fits declLength cMaxColumns then
    noop block
  else
    change block (const printDecl)

  where
    originalDeclBlock =
      Block (getStartLineUnsafe ldecl) (getEndLineUnsafe ldecl)

    printerConfig = PrinterConfig
      { columns = case cMaxColumns of
          NoMaxColumns -> Nothing
          MaxColumns n -> Just n
      }

    printDecl = runPrinter_ printerConfig [] m do
      (putRdrName $ sigName decl) >> space >> putText "::" >> newline
      spaces 5 >> (putRdrName $ head $ sigParameters decl) >> newline
      traverse (\para -> spaces 2 >> putText "->" >> space >> (putRdrName para) >> newline) (tail $ sigParameters decl)
