module Language.Haskell.Stylish.Printer.Module
  ( printModule
  ) where

import           Language.Haskell.Stylish.Module
import           Language.Haskell.Stylish.Config (Config'(..))
import           Language.Haskell.Stylish.Printer.Decl (printDecls)
import           Language.Haskell.Stylish.Printer.ModuleHeader (printModuleHeader)
import           Language.Haskell.Stylish.Printer.Imports (printImports)

printModule :: Config' -> Lines -> Module -> Lines
printModule c ls m =
  let
    comments = moduleComments m
  in
    printModuleHeader c ls comments (moduleHeader m) <>
    printImports c ls comments (moduleImports m) <>
    printDecls c ls comments (moduleDecls m) <>
    []
