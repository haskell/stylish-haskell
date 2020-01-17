module Language.Haskell.Stylish.Step.Data where

import           Data.Maybe                      (maybeToList)
import qualified Language.Haskell.Exts           as H
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

type ChangeLine = Change String

step :: Int -> Step
step indent = makeStep "Data" (step' indent)

step' :: Int -> Lines -> Module -> Lines
step' indent ls (module', _) = applyChanges changes ls
  where
    datas' = datas $ fmap linesFromSrcSpan module'
    changes = datas' >>= (maybeToList . (changeDecl indent))

changeDecl :: Int -> (LineBlock, H.Decl l)  -> Maybe ChangeLine
changeDecl indent (block, H.DataDecl _ (H.DataType _) _ dhead decls _) =
  Just $ change block (const $ concat newLines)
  where
    zipped = zip decls [1..]
    newLines = fmap (\(decl, i) -> if (i == 1) then processConstructor typeConstructor decl else processConstructor (indented "| ") decl) zipped
    typeConstructor = "data " <> H.prettyPrint dhead <> " = "
    firstName (fname, _type) = indented "{ " <> H.prettyPrint fname <> " :: " <> H.prettyPrint _type
    otherName (fname, _type) = indented ", " <> H.prettyPrint fname <> " :: " <> H.prettyPrint _type
    extractField (H.FieldDecl _ names _type) = (head names, _type)

    processConstructor init (H.QualConDecl _ _ _ (H.RecDecl _ dname fields)) = do
      init <> H.prettyPrint dname : (firstName $ extractField $ head fields) : (fmap (otherName . extractField) (tail fields)) ++ [indented "}"]
    processConstructor init decl = [init <> trimLeft (H.prettyPrint decl)]

    indented str = (replicate indent ' ') <> str
changeDecl _ _ = Nothing
