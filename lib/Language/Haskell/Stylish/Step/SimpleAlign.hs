--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Step.SimpleAlign
    ( Config (..)
    , defaultConfig
    , step
    ) where


--------------------------------------------------------------------------------
import           Data.Data                       (Data)
import           Data.Maybe                      (maybeToList)
import qualified Language.Haskell.Exts.Annotated as H


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Align
import           Language.Haskell.Stylish.Editor
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Util


--------------------------------------------------------------------------------
data Config = Config
    { cCases            :: !Bool
    , cTopLevelPatterns :: !Bool
    , cRecords          :: !Bool
    } deriving (Show)


--------------------------------------------------------------------------------
defaultConfig :: Config
defaultConfig = Config
    { cCases            = True
    , cTopLevelPatterns = True
    , cRecords          = True
    }


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
records :: H.Module l -> [[H.FieldDecl l]]
records modu =
    [ fields
    | H.Module _ _ _ _ decls                     <- [modu]
    , H.DataDecl _ _ _ _ cons _                  <- decls
    , H.QualConDecl _ _ _ (H.RecDecl _ _ fields) <- cons
    ]


--------------------------------------------------------------------------------
fieldDeclToAlignable :: H.FieldDecl a -> Maybe (Alignable a)
fieldDeclToAlignable (H.FieldDecl ann names ty) = Just $ Alignable
    { aContainer = ann
    , aLeft      = H.ann (last names)
    , aRight     = H.ann ty
    , aRightLead = length ":: "
    }


--------------------------------------------------------------------------------
step :: Int -> Config -> Step
step maxColumns config = makeStep "Cases" $ \ls (module', _) ->
    let module''               = fmap H.srcInfoSpan module'
        changes search toAlign =
            [ change_
            | case_   <- search module''
            , aligns  <- maybeToList (mapM toAlign case_)
            , change_ <- align maxColumns aligns
            ]

        configured             = concat $
            [changes cases   altToAlignable       | cCases config] ++
            [changes tlpats  matchToAlignable     | cTopLevelPatterns config] ++
            [changes records fieldDeclToAlignable | cRecords config]

    in applyChanges configured ls
