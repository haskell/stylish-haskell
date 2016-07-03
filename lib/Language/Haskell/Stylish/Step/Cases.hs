--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Step.Cases
    ( step
    ) where


--------------------------------------------------------------------------------
import           Data.Data                       (Data)
import           Data.Maybe                       (maybeToList)
import qualified Language.Haskell.Exts.Annotated as H


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Align
import           Language.Haskell.Stylish.Editor
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Util


--------------------------------------------------------------------------------
cases :: Data l => H.Module l -> [[H.Alt l]]
cases modu = [alts | H.Case _ _ alts <- everything modu]


--------------------------------------------------------------------------------
altToAlignable :: H.Alt l -> Maybe (Alignable l)
altToAlignable (H.Alt _   _   _   (Just _)) = Nothing
altToAlignable (H.Alt ann pat rhs Nothing)  = Just $ Alignable
    { aContainer = ann
    , aLeft      = H.ann pat
    , aRight     = H.ann rhs
    , aRightLead = length "-> "
    }


--------------------------------------------------------------------------------
step :: Int -> Step
step maxColumns = makeStep "Cases" $ \ls (module', _) ->
    let module'' = fmap H.srcInfoSpan module' in
    applyChanges
        [ change_
        | case_   <- cases module''
        , aligns  <- maybeToList (mapM altToAlignable case_)
        , change_ <- align maxColumns aligns
        ]
        ls
