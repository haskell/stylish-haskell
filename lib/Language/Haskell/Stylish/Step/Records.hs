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
    -- | This is the minimal number of columns we need for the leading part not
    -- included in our right string.  For example, for datatype alignment, this
    -- leading part is the string ":: " so we use 3.
    , aRightLead :: !Int
    } deriving (Show)


--------------------------------------------------------------------------------
fieldDeclToAlignable :: H.FieldDecl a -> Alignable a
fieldDeclToAlignable (H.FieldDecl ann names ty) = Alignable
    { aContainer = ann
    , aLeft      = H.ann (last names)
    , aRight     = H.ann ty
    , aRightLead = length ":: "
    }


--------------------------------------------------------------------------------
-- | Align the type of a field
align :: Int -> [Alignable H.SrcSpan] -> [Change String]
align maxColumns alignment
    -- Do not make any change if we would go past the maximum number of columns.
    | longestLeft + longestRight > maxColumns = info []
    | otherwise                               = info $ map align' alignment
  where
    info =
        id
        -- trace ("Alignable: " ++ show alignment) .
        -- trace ("longestLeft: " ++ show longestLeft) .
        -- trace ("longestRight: " ++ show longestRight)

    -- The longest thing in the left column.
    longestLeft = maximum $ map (H.srcSpanEndColumn . aLeft) alignment

    -- The longest thing in the right column.
    longestRight = maximum
        [ H.srcSpanEndColumn (aRight a) - H.srcSpanStartColumn (aRight a)
            + aRightLead a
        | a <- alignment
        ]

    align' a = changeLine (H.srcSpanStartLine $ aContainer a) $ \str ->
        let column      = H.srcSpanEndColumn $ aLeft a
            (pre, post) = splitAt column str
        in [padRight longestLeft (trimRight pre) ++ trimLeft post]

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
step :: Int -> Step
step maxColumns = makeStep "Records" $ \ls (module', _) ->
    let module''       = fmap H.srcInfoSpan module'
        fixableRecords = filter fixable $ records module''
    in applyChanges (fixableRecords >>= align maxColumns) ls
