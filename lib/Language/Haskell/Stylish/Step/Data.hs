module Language.Haskell.Stylish.Step.Data where

import           Data.List                       (intercalate)
import           Data.Maybe                      (maybeToList)
import qualified Language.Haskell.Exts           as H
import           Language.Haskell.Stylish.Block
import           Language.Haskell.Stylish.Editor
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Util
import           Prelude                         hiding (init)

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
    changes = datas' >>= maybeToList . changeDecl indentSize

changeDecl :: Int -> (LineBlock, H.Decl l)  -> Maybe ChangeLine
changeDecl _ (_, H.DataDecl _ (H.DataType _) Nothing _ [] _) = Nothing
changeDecl indentSize (block, H.DataDecl _ (H.DataType _) Nothing dhead decls derivings) =
  Just $ change block (const $ concat newLines)
  where
    newLines = fmap constructors zipped ++ [fmap (indented . H.prettyPrint) derivings]
    zipped = zip decls ([1..] ::[Int])
    constructors (decl, 1) = processConstructor typeConstructor indentSize decl
    constructors (decl, _) = processConstructor (indented "| ") indentSize decl
    typeConstructor = "data " <> H.prettyPrint dhead <> " = "
    indented = indent indentSize
changeDecl _ _ = Nothing

processConstructor :: String -> Int -> H.QualConDecl l -> [String]
processConstructor init indentSize (H.QualConDecl _ _ _ (H.RecDecl _ dname fields)) =
  init <> H.prettyPrint dname : processName "{ " ( extractField $ head fields) : fmap (processName ", " . extractField) (tail fields) ++ [indented "}"]
  where
    processName prefix (fnames, _type) =
      indented prefix <> intercalate ", " (fmap H.prettyPrint fnames) <> " :: " <> H.prettyPrint _type
    extractField (H.FieldDecl _ names _type) = (names, _type)
    indented = indent indentSize
processConstructor init _ decl = [init <> trimLeft (H.prettyPrint decl)]
