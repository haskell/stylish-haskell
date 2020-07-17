--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Step.SimpleAlign
    ( Config (..)
    , defaultConfig
    , step
    ) where


--------------------------------------------------------------------------------
import           Data.Data                       (Data)
import           Data.List                       (foldl')
import           Data.Maybe                      (maybeToList)
import qualified Language.Haskell.Exts           as H


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
-- | For this to work well, we require a way to merge annotations.  This merge
-- operation should follow the semigroup laws.
altToAlignable :: (l -> l -> l) -> H.Alt l -> Maybe (Alignable l)
altToAlignable _ (H.Alt _   _   _   (Just _)) = Nothing
altToAlignable _ (H.Alt ann pat rhs@(H.UnGuardedRhs _ _) Nothing) = Just $
    Alignable
        { aContainer = ann
        , aLeft      = H.ann pat
        , aRight     = H.ann rhs
        , aRightLead = length "-> "
        }
altToAlignable
        merge
        (H.Alt ann pat (H.GuardedRhss _ [H.GuardedRhs _ guards rhs]) Nothing) =
    -- We currently only support the case where an alternative has a single
    -- guarded RHS.  If there are more, we would need to return multiple
    -- `Alignable`s from this function, which would be a significant change.
    Just $ Alignable
        { aContainer = ann
        , aLeft      = foldl' merge (H.ann pat) (map H.ann guards)
        , aRight     = H.ann rhs
        , aRightLead = length "-> "
        }
altToAlignable _ _ = Nothing


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
step :: Maybe Int -> Config -> Step
step maxColumns config = oldMakeStep "Cases" $ \ls (module', _) ->
    let module''               = fmap H.srcInfoSpan module'
        changes search toAlign =
            [ change_
            | case_   <- search module''
            , aligns  <- maybeToList (mapM toAlign case_)
            , change_ <- align maxColumns aligns
            ]

        configured = concat $
            [ changes cases (altToAlignable H.mergeSrcSpan)
            | cCases config
            ] ++
            [changes tlpats  matchToAlignable | cTopLevelPatterns config] ++
            [changes records fieldDeclToAlignable | cRecords config]

    in applyChanges configured ls
