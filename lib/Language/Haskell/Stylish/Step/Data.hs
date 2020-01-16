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
changeDecl (block, H.DataDecl _ (H.DataType _) _ (H.DHead _ ident) qualConDecls _) = do
  (H.QualConDecl _ _ _ (H.RecDecl _ dname fields)) <- qualConDecls
  (H.FieldDecl _ names _type) <- fields
  fname <- names
  [change block (\_ ->
                   ["data " <> H.prettyPrint ident <> " = " <> H.prettyPrint dname
                   ,"  { " <> H.prettyPrint fname <> " :: " <> H.prettyPrint _type
                   ,"  }"
                   ]
                )]
changeDecl _ = []
