module Language.Haskell.Stylish.Step.Data where

import           Data.Maybe                      (maybeToList)
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
    changes = datas' >>= (maybeToList . changeDecl)

changeDecl :: (LineBlock, H.Decl l)  -> Maybe ChangeLine
changeDecl (block, H.DataDecl _ (H.DataType _) _ dhead [(H.QualConDecl _ _ _ (H.RecDecl _ dname fields))] _) =
  Just $ change block (const newLines)
  where
    newLines = typeConstructor : (firstName $ extractField $ head fields) : (fmap (otherName . extractField) (tail fields)) ++ ["  }"]
    typeConstructor = "data " <> H.prettyPrint dhead <> " = " <> H.prettyPrint dname
    firstName (fname, _type) = "  { " <> H.prettyPrint fname <> " :: " <> H.prettyPrint _type
    otherName (fname, _type) = "  , " <> H.prettyPrint fname <> " :: " <> H.prettyPrint _type
    extractField (H.FieldDecl _ names _type) = (head names, _type)
changeDecl _ = Nothing
