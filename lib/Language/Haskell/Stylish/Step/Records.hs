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
records :: H.Module l -> [[Alignable l]]
records modu =
    [ map fieldDeclToAlignable fields
    | H.Module _ _ _ _ decls                     <- [modu]
    , H.DataDecl _ _ _ _ cons _                  <- decls
    , H.QualConDecl _ _ _ (H.RecDecl _ _ fields) <- cons
    ]


--------------------------------------------------------------------------------
data Alignable a = Alignable
    { aContainer :: !a
    , aLeft      :: !a
    , aRight     :: !a
    } deriving (Show)


--------------------------------------------------------------------------------
fieldDeclToAlignable :: H.FieldDecl a -> Alignable a
fieldDeclToAlignable (H.FieldDecl ann names ty) = Alignable
    { aContainer = ann
    , aLeft      = H.ann (last names)
    , aRight     = H.ann ty
    }


--------------------------------------------------------------------------------
-- | Align the type of a field
align :: [Alignable H.SrcSpan] -> [Change String]
align alignment = map align' alignment
  where
    longest = maximum $ map (H.srcSpanEndColumn . aLeft) alignment

    align' a = changeLine (H.srcSpanStartLine $ aContainer a) $ \str ->
        let column      = H.srcSpanEndColumn $ aLeft a
            (pre, post) = splitAt column str
        in [padRight longest (trimRight pre) ++ trimLeft post]

    trimLeft  = dropWhile isSpace
    trimRight = reverse . trimLeft . reverse


--------------------------------------------------------------------------------
-- | Checks that all no field of the record appears on more than one line,
-- amonst other things
fixable :: [Alignable H.SrcSpan] -> Bool
fixable []     = False
fixable fields = all singleLine containers && nonOverlapping containers
  where
    containers        = map aContainer fields
    singleLine s      = H.srcSpanStartLine s == H.srcSpanEndLine s
    nonOverlapping ss = length ss == length (nub $ map H.srcSpanStartLine ss)


--------------------------------------------------------------------------------
step :: Step
step = makeStep "Records" $ \ls (module', _) ->
    let module''       = fmap H.srcInfoSpan module'
        fixableRecords = filter fixable $ records module''
    in applyChanges (fixableRecords >>= align) ls
