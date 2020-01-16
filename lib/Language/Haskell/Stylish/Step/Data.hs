module Language.Haskell.Stylish.Step.Data where

import qualified Language.Haskell.Exts           as H
import qualified Language.Haskell.Exts.Syntax    as H

import           Language.Haskell.Stylish.Block
import           Language.Haskell.Stylish.Editor
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Util

datas :: H.Module l -> [(l, H.Decl l)]
datas modu =
    [ (l, H.DataDecl l b c d e f)
    | H.Module _ _ _ _ decls                  <- [modu]
    , H.DataDecl l b c d e f                  <- decls
    ]


step :: Step
step = makeStep "Data" step'

step' :: Lines -> Module -> Lines
step' ls (module', _) = applyChanges changes ls
  where
    datas' = datas $ fmap linesFromSrcSpan module'
    changes = fmap (delete . fst) datas'

prettyDataDecls = undefined
