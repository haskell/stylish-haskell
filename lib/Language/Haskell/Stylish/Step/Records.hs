--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Step.Records
    ( step
    ) where


--------------------------------------------------------------------------------
import qualified Language.Haskell.Exts.Annotated as H


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Align
import           Language.Haskell.Stylish.Editor
import           Language.Haskell.Stylish.Step


--------------------------------------------------------------------------------
records :: H.Module l -> [[Alignable l]]
records modu =
    [ map fieldDeclToAlignable fields
    | H.Module _ _ _ _ decls                     <- [modu]
    , H.DataDecl _ _ _ _ cons _                  <- decls
    , H.QualConDecl _ _ _ (H.RecDecl _ _ fields) <- cons
    ]


--------------------------------------------------------------------------------
fieldDeclToAlignable :: H.FieldDecl a -> Alignable a
fieldDeclToAlignable (H.FieldDecl ann names ty) = Alignable
    { aContainer = ann
    , aLeft      = H.ann (last names)
    , aRight     = H.ann ty
    , aRightLead = length ":: "
    }


--------------------------------------------------------------------------------
step :: Int -> Step
step maxColumns = makeStep "Records" $ \ls (module', _) ->
    let module'' = fmap H.srcInfoSpan module' in
    applyChanges (records module'' >>= align maxColumns) ls
