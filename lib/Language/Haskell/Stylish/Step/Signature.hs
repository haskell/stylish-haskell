module Language.Haskell.Stylish.Step.Signature where

import           Language.Haskell.Stylish.Step

data Config = Config
  { maxColumnLength :: Int
  }

step :: Config -> Step
step _ = makeStep "Signature" (\ls _ -> ls)
