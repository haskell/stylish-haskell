--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Step.Squash
    ( step
    ) where


--------------------------------------------------------------------------------
import           Data.Maybe                      (mapMaybe)
import qualified Language.Haskell.Exts           as H


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Editor
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Util


--------------------------------------------------------------------------------
squash
    :: (H.Annotated l, H.Annotated r)
    => l H.SrcSpan -> r H.SrcSpan -> Maybe (Change String)
squash left right
  | H.srcSpanEndLine lAnn == H.srcSpanStartLine rAnn = Just $
        changeLine (H.srcSpanEndLine lAnn) $ \str ->
        let (pre, post) = splitAt (H.srcSpanEndColumn lAnn) str
        in  [trimRight pre ++ " " ++ trimLeft post]
  | otherwise = Nothing
  where
    lAnn = H.ann left
    rAnn = H.ann right


--------------------------------------------------------------------------------
squashFieldDecl :: H.FieldDecl H.SrcSpan -> Maybe (Change String)
squashFieldDecl (H.FieldDecl _ names type')
  | null names = Nothing
  | otherwise  = squash (last names) type'


--------------------------------------------------------------------------------
squashMatch :: H.Match H.SrcSpan -> Maybe (Change String)
squashMatch (H.InfixMatch _ _ _ _ _ _) = Nothing
squashMatch (H.Match _ name pats rhs _)
  | null pats = squash name        rhs
  | otherwise = squash (last pats) rhs


--------------------------------------------------------------------------------
squashAlt :: H.Alt H.SrcSpan -> Maybe (Change String)
squashAlt (H.Alt _ pat rhs _) = squash pat rhs


--------------------------------------------------------------------------------
step :: Step
step = oldMakeStep "Squash" $ \ls (module', _) ->
    let module'' = fmap H.srcInfoSpan module'
        changes  = concat
            [ mapMaybe squashAlt       (everything module'')
            , mapMaybe squashMatch     (everything module'')
            , mapMaybe squashFieldDecl (everything module'')
            ]
    in applyChanges changes ls
