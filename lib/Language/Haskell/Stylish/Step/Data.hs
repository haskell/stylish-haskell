module Language.Haskell.Stylish.Step.Data where

import           Data.List                       (find, intercalate)
import           Data.Maybe                      (isNothing, maybeToList)
import qualified Language.Haskell.Exts           as H
import           Language.Haskell.Exts.Comments
import           Language.Haskell.Stylish.Block
import           Language.Haskell.Stylish.Editor
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Util
import           Prelude                         hiding (init)

datas :: H.Module l -> [H.Decl l]
datas (H.Module _ _ _ _ decls) = decls
datas _                        = []

type ChangeLine = Change String

step :: Int -> Step
step indentSize = makeStep "Data" (step' indentSize)

step' :: Int -> Lines -> Module -> Lines
step' indentSize ls (module', allComments) = applyChanges changes ls
  where
    datas' = datas $ fmap linesFromSrcSpan module'
    changes = datas' >>= maybeToList . changeDecl allComments indentSize

findComment :: LineBlock -> [Comment] -> Maybe Comment
findComment lb = find foo
  where
    foo (Comment _ (H.SrcSpan _ start _ end _) _) =
      blockStart lb == start && blockEnd lb == end

commentWithin :: LineBlock -> [Comment] -> Maybe Comment
commentWithin lb = find foo
  where
    foo (Comment _ (H.SrcSpan _ start _ end _) _) =
      start >= blockStart lb && end <= blockEnd lb

changeDecl :: [Comment] -> Int -> H.Decl LineBlock -> Maybe ChangeLine
changeDecl _ _ (H.DataDecl _ (H.DataType _) Nothing _ [] _) = Nothing
changeDecl allComments indentSize (H.DataDecl block (H.DataType _) Nothing dhead decls derivings)
  | isNothing $ commentWithin block allComments = fmap (\l -> change block (const $ concat l)) newLines
  | otherwise = Nothing
  where
    newLines = if Nothing `elem` maybeConstructors then Nothing
       else Just $ fmap concat maybeConstructors ++ [fmap (indented . H.prettyPrint) derivings]
    maybeConstructors :: [Maybe [String]]
    maybeConstructors = fmap constructors zipped
    zipped = zip decls ([1..] ::[Int])
    constructors (decl, 1) = processConstructor allComments typeConstructor indentSize decl
    constructors (decl, _) = processConstructor allComments (indented "| ") indentSize decl
    typeConstructor = "data " <> H.prettyPrint dhead <> " = "
    indented = indent indentSize
changeDecl _ _ _ = Nothing

processConstructor :: [Comment] -> String -> Int -> H.QualConDecl LineBlock -> Maybe [String]
processConstructor allComments init indentSize (H.QualConDecl _ _ _ (H.RecDecl _ dname fields)) = do
  n1 <- processName "{ " ( extractField $ head fields)
  ns <- traverse (processName ", " . extractField) (tail fields)
  Just $ init <> H.prettyPrint dname : n1 : ns ++ [indented "}"]
  where
    processName prefix (fnames, _type, Nothing) = Just $
      indented prefix <> intercalate ", " (fmap H.prettyPrint fnames) <> " :: " <> H.prettyPrint _type
    processName prefix (fnames, _type, (Just (Comment _ _ c))) = Just $
      indented prefix <> intercalate ", " (fmap H.prettyPrint fnames) <> " :: " <> H.prettyPrint _type <> " --" <> c
    extractField (H.FieldDecl lb names _type) = (names, _type, findComment lb allComments)
    indented = indent indentSize
processConstructor _ init _ decl = Just [init <> trimLeft (H.prettyPrint decl)]
