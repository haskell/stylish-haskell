module Language.Haskell.Stylish.Step.Data where

import           Prelude hiding (init)
import           Data.List (intercalate)
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
step indentSize = makeStep "Data" (step' indentSize)

step' :: Int -> Lines -> Module -> Lines
step' indentSize ls (module', _) = applyChanges changes ls
  where
    datas' = datas $ fmap linesFromSrcSpan module'
    changes = datas' >>= (maybeToList . (changeDecl indentSize))

changeDecl :: Int -> (LineBlock, H.Decl l)  -> Maybe ChangeLine
changeDecl _ (_, H.DataDecl _ (H.DataType _) Nothing _ [] _) = Nothing
changeDecl indentSize (block, H.DataDecl _ (H.DataType _) Nothing dhead decls derivings) =
  Just $ change block (const $ concat newLines)
  where
    zipped = zip decls ([1..] ::[Int])
    newLines :: [[String]]
    newLines = (fmap (\(decl, i) -> if (i == 1) then processConstructor typeConstructor decl else processConstructor (indented "| ") decl) zipped) ++ [fmap (\d -> indented $ H.prettyPrint d) derivings]
    typeConstructor = "data " <> H.prettyPrint dhead <> " = "
    firstName (fnames, _type) = indented "{ " <> intercalate ", " (fmap H.prettyPrint fnames) <> " :: " <> H.prettyPrint _type
    otherName (fnames, _type) = indented ", " <> intercalate ", " (fmap H.prettyPrint fnames) <> " :: " <> H.prettyPrint _type
    extractField (H.FieldDecl _ names _type) = (names, _type)

    processConstructor init (H.QualConDecl _ _ _ (H.RecDecl _ dname fields)) = do
      init <> H.prettyPrint dname : (firstName $ extractField $ head fields) : (fmap (otherName . extractField) (tail fields)) ++ [indented "}"]
    processConstructor init decl = [init <> trimLeft (H.prettyPrint decl)]

    indented str = indent indentSize str

changeDecl _ _ = Nothing
