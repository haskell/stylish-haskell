--------------------------------------------------------------------------------
-- | This module is useful for aligning things.
module Language.Haskell.Stylish.Align
    ( Alignable (..)
    , align
    ) where


--------------------------------------------------------------------------------
import           Data.List                       (nub)
import qualified SrcLoc                          as S


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Editor
import           Language.Haskell.Stylish.Util


--------------------------------------------------------------------------------
-- | This represent a single line which can be aligned.  We have something on
-- the left and the right side, e.g.:
--
-- > [x]  -> x + 1
-- > ^^^^    ^^^^^
-- > LEFT    RIGHT
--
-- We also have the container which holds the entire line:
--
-- > [x]  -> x + 1
-- > ^^^^^^^^^^^^^
-- > CONTAINER
--
-- And then we have a "right lead" which is just represented by an 'Int', since
-- @haskell-src-exts@ often does not allow us to access it.  In the example this
-- is:
--
-- > [x]  -> x + 1
-- >      ^^^
-- >      RLEAD
--
-- This info is enough to align a bunch of these lines.  Users of this module
-- should construct a list of 'Alignable's representing whatever they want to
-- align, and then call 'align' on that.
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
-- | Create changes that perform the alignment.

align
  :: Maybe Int                    -- ^ Max columns
  -> [Alignable S.RealSrcSpan]    -- ^ Alignables
  -> [Change String]              -- ^ Changes performing the alignment
align _ [] = []
align maxColumns alignment
  -- Do not make an changes if we would go past the maximum number of columns
  | exceedsColumns (longestLeft + longestRight)  = []
  | not (fixable alignment)                      = []
  | otherwise                                    = map align' alignment
  where
    exceedsColumns i = case maxColumns of
      Nothing -> False
      Just c  -> i > c

    -- The longest thing in the left column
    longestLeft = maximum $ map (S.srcSpanEndCol . aLeft) alignment

    -- The longest thing in the right column
    longestRight = maximum
      [ S.srcSpanEndCol (aRight a) - S.srcSpanStartCol (aRight a)
          + aRightLead a
      | a <- alignment
      ]

    align' a = changeLine (S.srcSpanStartLine $ aContainer a) $ \str ->
      let column = S.srcSpanEndCol $ aLeft a
          (pre, post) = splitAt column str
      in [padRight longestLeft (trimRight pre) ++ trimLeft post] 

--------------------------------------------------------------------------------
-- | Checks that all the alignables appear on a single line, and that they do
-- not overlap.

fixable :: [Alignable S.RealSrcSpan] -> Bool
fixable []     = False
fixable [_]    = False
fixable fields = all singleLine containers && nonOverlapping containers
  where
    containers        = map aContainer fields
    singleLine s      = S.srcSpanStartLine s == S.srcSpanEndLine s
    nonOverlapping ss = length ss == length (nub $ map S.srcSpanStartLine ss)
