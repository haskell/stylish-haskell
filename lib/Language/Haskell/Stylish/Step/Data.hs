{-# LANGUAGE RecordWildCards #-}

module Language.Haskell.Stylish.Step.Data where

import           Data.List                       (find, intercalate)
import           Data.Maybe                      (fromMaybe, maybeToList)
import qualified Language.Haskell.Exts           as H
import           Language.Haskell.Exts.Comments
import           Language.Haskell.Stylish.Block
import           Language.Haskell.Stylish.Editor
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Util
import           Prelude                         hiding (init)

data Indent
    = SameLine
    | Indent !Int
  deriving (Show)

data Config = Config
    { cEquals           :: !Indent
      -- ^ Indent between type constructor and @=@ sign (measured from column 0)
    , cFirstField       :: !Indent
      -- ^ Indent between data constructor and @{@ line (measured from column with data constructor name)
    , cFieldComment     :: !Int
      -- ^ Indent between column with @{@ and start of field line comment (this line has @cFieldComment = 2@)
    , cDeriving         :: !Int
      -- ^ Indent before @deriving@ lines (measured from column 0)
    } deriving (Show)

datas :: H.Module l -> [H.Decl l]
datas (H.Module _ _ _ _ decls) = decls
datas _                        = []

type ChangeLine = Change String

step :: Config -> Step
step cfg = makeStep "Data" . Left $ (step' cfg)

step' :: Config -> Lines -> Module -> Lines
step' cfg ls (module', allComments) = applyChanges changes ls
  where
    datas' = datas $ fmap linesFromSrcSpan module'
    changes = datas' >>= maybeToList . changeDecl allComments cfg

findCommentOnLine :: LineBlock -> [Comment] -> Maybe Comment
findCommentOnLine lb = find commentOnLine
  where
    commentOnLine (Comment _ (H.SrcSpan _ start _ end _) _) =
      blockStart lb == start && blockEnd lb == end

findCommentBelowLine :: LineBlock -> [Comment] -> Maybe Comment
findCommentBelowLine lb = find commentOnLine
  where
    commentOnLine (Comment _ (H.SrcSpan _ start _ end _) _) =
      blockStart lb == start - 1 && blockEnd lb == end - 1

commentsWithin :: LineBlock -> [Comment] -> [Comment]
commentsWithin lb = filter within
  where
    within (Comment _ (H.SrcSpan _ start _ end _) _) =
      start >= blockStart lb && end <= blockEnd lb

changeDecl :: [Comment] -> Config -> H.Decl LineBlock -> Maybe ChangeLine
changeDecl _ _ (H.DataDecl _ (H.DataType _) Nothing _ [] _) = Nothing
changeDecl allComments cfg@Config{..} (H.DataDecl block (H.DataType _) Nothing dhead decls derivings)
  | hasRecordFields = Just $ change block (const $ concat newLines)
  | otherwise       = Nothing
  where
    hasRecordFields = any
      (\qual -> case qual of
                  (H.QualConDecl _ _ _ (H.RecDecl {})) -> True
                  _ -> False)
      decls

    typeConstructor = "data " <> H.prettyPrint dhead

    -- In any case set @pipeIndent@ such that @|@ is aligned with @=@.
    (firstLine, firstLineInit, pipeIndent) =
      case cEquals of
        SameLine -> (Nothing, typeConstructor <> " = ", length typeConstructor + 1)
        Indent n -> (Just [[typeConstructor]], indent n "= ", n)

    newLines = fromMaybe [] firstLine ++ fmap constructors zipped <> [fmap (indent cDeriving . H.prettyPrint) derivings]
    zipped = zip decls ([1..] ::[Int])

    constructors (decl, 1) = processConstructor allComments firstLineInit cfg decl
    constructors (decl, _) = processConstructor allComments (indent pipeIndent "| ") cfg decl
changeDecl _ _ _ = Nothing

processConstructor :: [Comment] -> String -> Config -> H.QualConDecl LineBlock -> [String]
processConstructor allComments init Config{..} (H.QualConDecl _ _ _ (H.RecDecl _ dname (f:fs))) = do
  fromMaybe [] firstLine <> n1 <> ns <> [indent fieldIndent "}"]
  where
    n1 = processName firstLinePrefix (extractField f)
    ns = fs >>= processName (indent fieldIndent ", ") . extractField

    -- Set @fieldIndent@ such that @,@ is aligned with @{@.
    (firstLine, firstLinePrefix, fieldIndent) =
      case cFirstField of
        SameLine ->
          ( Nothing
          , init <> H.prettyPrint dname <> " { "
          , length init + length (H.prettyPrint dname) + 1
          )
        Indent n ->
          ( Just [init <> H.prettyPrint dname]
          , indent (length init + n) "{ "
          , length init + n
          )

    processName prefix (fnames, _type, lineComment, commentBelowLine) =
      [prefix <> intercalate ", " (fmap H.prettyPrint fnames) <> " :: " <> H.prettyPrint _type <> addLineComment lineComment
      ] ++ addCommentBelow commentBelowLine

    addLineComment (Just (Comment _ _ c)) = " --" <> c
    addLineComment Nothing                = ""

    -- Field comment indent is measured from the column with @{@, hence adding of @fieldIndent@ here.
    addCommentBelow Nothing                = []
    addCommentBelow (Just (Comment _ _ c)) = [indent (fieldIndent + cFieldComment) "--" <> c]

    extractField (H.FieldDecl lb names _type) =
      (names, _type, findCommentOnLine lb allComments, findCommentBelowLine lb allComments)

processConstructor _ init _ decl = [init <> trimLeft (H.prettyPrint decl)]
