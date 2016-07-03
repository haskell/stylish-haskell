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
tlpats :: Data l => H.Module l -> [[H.Match l]]
tlpats modu = [matches | H.FunBind _ matches <- everything modu]


--------------------------------------------------------------------------------
matchToAlignable :: H.Match l -> Maybe (Alignable l)
matchToAlignable (H.InfixMatch _ _ _ _ _ _)           = Nothing
matchToAlignable (H.Match _   _    []   _   _)        = Nothing
matchToAlignable (H.Match _   _    _    _   (Just _)) = Nothing
matchToAlignable (H.Match ann name pats rhs Nothing)  = Just $ Alignable
    { aContainer = ann
    , aLeft      = last (H.ann name : map H.ann pats)
    , aRight     = H.ann rhs
    , aRightLead = length "= "
    }


--------------------------------------------------------------------------------
step :: Int -> Step
step maxColumns = makeStep "Cases" $ \ls (module', _) ->
    let module''               = fmap H.srcInfoSpan module'
        changes search toAlign =
            [ change_
            | case_   <- search module''
            , aligns  <- maybeToList (mapM toAlign case_)
            , change_ <- align maxColumns aligns
            ] in

    applyChanges
        (changes cases altToAlignable ++ changes tlpats matchToAlignable)
        ls
