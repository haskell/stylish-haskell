module Language.Haskell.Stylish.Step.Data where

import           Data.List                       (intercalate)
import           Data.Maybe                      (maybeToList)
import qualified Language.Haskell.Exts           as H
import           Language.Haskell.Exts.Comments
import           Language.Haskell.Stylish.Block
import           Language.Haskell.Stylish.Editor
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Util
import           Prelude                         hiding (init)

datas :: H.Module (l, c) -> [H.Decl (l, c)]
datas (H.Module _ _ _ _ decls) = decls
datas _                        = []

type ChangeLine = Change String

step :: Int -> Step
step indentSize = makeStep "Data" (step' indentSize)

step' :: Int -> Lines -> Module -> Lines
step' indentSize ls module' = applyChanges changes ls
  where
    module'' = associateHaddock module'
    datas' = datas $ fmap (\(i, c) -> (linesFromSrcSpan i, c)) module''
    changes = datas' >>= maybeToList . changeDecl indentSize

changeDecl :: Int -> H.Decl (LineBlock, [Comment]) -> Maybe ChangeLine
changeDecl _ (H.DataDecl _ (H.DataType _) Nothing _ [] _) = Nothing
changeDecl indentSize (H.DataDecl (block, _) (H.DataType _) Nothing dhead decls derivings) =
  Just $ change block (const $ concat newLines)
  where
    newLines = fmap constructors zipped ++ [fmap (indented . H.prettyPrint) derivings]
    zipped = zip decls ([1..] ::[Int])
    constructors (decl, 1) = processConstructor typeConstructor indentSize decl
    constructors (decl, _) = processConstructor (indented "| ") indentSize decl
    typeConstructor = "data " <> H.prettyPrint dhead <> " = "
    indented = indent indentSize
changeDecl _ _ = Nothing

processConstructor :: String -> Int -> H.QualConDecl (LineBlock, [Comment]) -> [String]
processConstructor init indentSize (H.QualConDecl _ _ _ (H.RecDecl _ dname fields)) =
  init <> H.prettyPrint dname : processName "{ " ( extractField $ head fields) : fmap (processName ", " . extractField) (tail fields) ++ [indented "}"]
  where
    processName prefix (fnames, _type, comments) =
      indented prefix <> intercalate ", " (fmap H.prettyPrint fnames) <> " :: " <> H.prettyPrint _type <> (concat $ fmap show comments)
    extractField (H.FieldDecl (_, comments) names _type) = (names, _type, comments)
    indented = indent indentSize
processConstructor init _ decl = [init <> trimLeft (H.prettyPrint decl)]
