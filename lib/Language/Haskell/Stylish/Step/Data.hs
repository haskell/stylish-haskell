module Language.Haskell.Stylish.Step.Data where

import qualified Language.Haskell.Exts           as H

import           Language.Haskell.Stylish.Block
import           Language.Haskell.Stylish.Editor
import           Language.Haskell.Stylish.Step

datas :: H.Module l -> [(l, H.Decl l)]
datas modu =
    [ (l, H.DataDecl l b c d e f)
    | H.Module _ _ _ _ decls                  <- [modu]
    , H.DataDecl l b c d e f                  <- decls
    ]

type ChangeLine = Change String

step :: Step
step = makeStep "Data" step'

step' :: Lines -> Module -> Lines
step' ls (module', _) = applyChanges changes ls
  where
    datas' = datas $ fmap linesFromSrcSpan module'
    changes = datas' >>= changeDecl

changeDecl :: (LineBlock, H.Decl l)  -> [ChangeLine]
changeDecl (block,  _) = [change block ("-- this is a comment" : )]
