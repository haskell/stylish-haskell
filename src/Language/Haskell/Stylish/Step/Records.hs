--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Step.Records
    ( step
    ) where


--------------------------------------------------------------------------------
import           Data.Char                       (isSpace)
import           Data.List                       (nub)
import qualified Language.Haskell.Exts.Annotated as H


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Editor
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Util


--------------------------------------------------------------------------------
records :: H.Module l -> [[H.FieldDecl l]]
records modu =
    [ fields
    | H.Module _ _ _ _ decls                     <- [modu]
    , H.DataDecl _ _ _ _ cons _                  <- decls
    , H.QualConDecl _ _ _ (H.RecDecl _ _ fields) <- cons
    ]


--------------------------------------------------------------------------------
-- | Align the type of a field
align :: [(Int, Int)] -> [Change String]
align alignment = map align' alignment
  where
    longest = maximum $ map snd alignment

    align' (line, column) = changeLine line $ \str ->
        let (pre, post) = splitAt column str
        in [padRight longest (trimRight pre) ++ trimLeft post]

    trimLeft  = dropWhile isSpace
    trimRight = reverse . trimLeft . reverse


--------------------------------------------------------------------------------
-- | Determine alignment of fields
fieldAlignment :: [H.FieldDecl H.SrcSpan] -> [(Int, Int)]
fieldAlignment fields =
    [ (H.srcSpanStartLine ann, H.srcSpanEndColumn ann)
    | H.FieldDecl _ names _ <- fields
    , let ann = H.ann (last names)
    ]


--------------------------------------------------------------------------------
-- | Checks that all no field of the record appears on more than one line,
-- amonst other things
fixable :: [H.FieldDecl H.SrcSpan] -> Bool
fixable []     = False
fixable fields = all singleLine srcSpans && nonOverlapping srcSpans
  where
    srcSpans          = map H.ann fields
    singleLine s      = H.srcSpanStartLine s == H.srcSpanEndLine s
    nonOverlapping ss = length ss == length (nub $ map H.srcSpanStartLine ss)


--------------------------------------------------------------------------------
step :: Step
step = makeStep "Records" $ \ls (module', _) ->
    let module''       = fmap H.srcInfoSpan module'
        fixableRecords = filter fixable $ records module''
    in applyChanges (fixableRecords >>= align . fieldAlignment) ls
