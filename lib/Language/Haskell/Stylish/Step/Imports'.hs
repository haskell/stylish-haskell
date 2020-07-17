module Language.Haskell.Stylish.Step.Imports'
  ( Config (..)
  , step
  ) where

--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Printer.Imports (printImports)

data Config = Config

step :: Config -> Step
step = makeStep "Imports" . printImports
