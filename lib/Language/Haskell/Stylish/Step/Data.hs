{-# LANGUAGE RecordWildCards #-}

module Language.Haskell.Stylish.Step.Data where

import           Data.List                       (find, init, intercalate)
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
    | IndentRelative !Int
    | IndentAbsolute !Int
  deriving (Show)

data Config = Config
    { cEquals       :: !Indent
      -- ^ Indent between type constructor and @=@ sign (measured from column 0, relative is ignored here)
    , cFirstField   :: !Indent
      -- ^ Indent between data constructor and @{@ line (measured from column 0 if absolute, from column with data constructor name if relative)
    , cFieldComment :: !Int
      -- ^ Indent between column with @{@ and start of field line comment (this line has @cFieldComment = 2@)
    , cDeriving     :: !Indent
      -- ^ Indent before @deriving@ lines (measured from column 0 if absolute, from column with data constructor name if relative)
    } deriving (Show)

datas :: H.Module l -> [H.Decl l]
datas (H.Module _ _ _ _ decls) = decls
datas _                        = []

type ChangeLine = Change String

step :: Config -> Step
step cfg = makeStep "Data" (step' cfg)

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
                  _                                    -> False)
      decls

    typeConstructor = "data " <> H.prettyPrint dhead

    -- In any case set @pipeIndent@ such that @|@ is aligned with @=@.
    (firstLine, firstLineInit, pipeIndent) =
      case cEquals of
        SameLine         -> (Nothing, typeConstructor <> " = ", length typeConstructor + 1)
        IndentRelative n -> (Just [[typeConstructor]], indent n "= ", n)
        IndentAbsolute n -> (Just [[typeConstructor]], indent n "= ", n)

    newLines = fromMaybe [] firstLine ++ addDerivings (fmap constructors zipped)
    zipped = zip decls ([1..] ::[Int])

    addDerivings constructorLines = case derivings of
      []     -> constructorLines
      (d:ds) -> case cDeriving of
        IndentAbsolute n -> constructorLines <> [fmap (indent n . H.prettyPrint) derivings]
        IndentRelative n -> constructorLines <> [fmap (indent (length firstLineInit + n) . H.prettyPrint) derivings]
        SameLine         -> let cLines = concat constructorLines
                                n      = length (last cLines) + 1
                            in [ init cLines <> [last cLines <> " " <> H.prettyPrint d]
                               ] <> [fmap (indent n . H.prettyPrint) ds]

    constructors (decl, 1) = processConstructor allComments firstLineInit cfg decl
    constructors (decl, _) = processConstructor allComments (indent pipeIndent "| ") cfg decl
changeDecl _ _ _ = Nothing

processConstructor :: [Comment] -> String -> Config -> H.QualConDecl LineBlock -> [String]
processConstructor allComments flInit Config{..} (H.QualConDecl _ _ _ (H.RecDecl _ dname (f:fs))) = do
  fromMaybe [] firstLine <> n1 <> ns <> [indent fieldIndent "}"]
  where
    n1 = processName firstLinePrefix (extractField f)
    ns = fs >>= processName (indent fieldIndent ", ") . extractField

    -- Set @fieldIndent@ such that @,@ is aligned with @{@.
    (firstLine, firstLinePrefix, fieldIndent) =
      case cFirstField of
        SameLine ->
          ( Nothing
          , flInit <> H.prettyPrint dname <> " { "
          , length flInit + length (H.prettyPrint dname) + 1
          )
        IndentRelative n ->
          ( Just [flInit <> H.prettyPrint dname]
          , indent (length flInit + n) "{ "
          , length flInit + n
          )
        IndentAbsolute n ->
          ( Just [flInit <> H.prettyPrint dname]
          , indent n "{ "
          , n
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

processConstructor _ flInit _ decl = [flInit <> trimLeft (H.prettyPrint decl)]
