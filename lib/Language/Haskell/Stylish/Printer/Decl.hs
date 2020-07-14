module Language.Haskell.Stylish.Printer.Decl
  ( printDecls
  ) where

--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Module
import           Language.Haskell.Stylish.Config (Config'(..))
--import           GHC.Hs.Decls
--import           SrcLoc                          (Located, GenLocated(..))
--import           GHC.Hs                          (GhcPs)

--------------------------------------------------------------------------------
printDecls :: Config' -> Lines -> Comments -> Decls -> Lines
printDecls _ ls _ _ = ls
