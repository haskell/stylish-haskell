module Language.Haskell.Stylish.Printer.Module
  ( printModule
  ) where

import           Language.Haskell.Stylish.Module
import           Language.Haskell.Stylish.Config (Config'(..))
import           Language.Haskell.Stylish.Printer.Decl (printDecls)
import           Language.Haskell.Stylish.Printer.ModuleHeader (printModuleHeader)
import           Language.Haskell.Stylish.Printer.Imports (printImports)

printModule :: Config' -> Module -> Lines
printModule c m =
  let
    comments = moduleComments m
  in
    printModuleHeader c comments (moduleHeader m) <>
    printImports c comments (moduleImports m) <>
    printDecls c comments (moduleDecls m) <>
    []
