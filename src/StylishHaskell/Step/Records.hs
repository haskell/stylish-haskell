--------------------------------------------------------------------------------
module StylishHaskell.Step.Records where


--------------------------------------------------------------------------------
import qualified Language.Haskell.Exts.Annotated as H


--------------------------------------------------------------------------------
import           StylishHaskell.Block


--------------------------------------------------------------------------------
records :: H.Module l -> [[H.FieldDecl l]]
records modu =
    [ fields
    | H.Module _ _ _ _ decls                     <- [modu]
    , H.DataDecl _ _ _ _ cons _                  <- decls
    , H.QualConDecl _ _ _ (H.RecDecl _ _ fields) <- cons
    ]


--------------------------------------------------------------------------------
fieldLines :: [H.FieldDecl H.SrcSpanInfo] -> Maybe [Int]
fieldLines rec
    | all singleLine blocks = Just $ map blockStart blocks
    | otherwise             = Nothing
  where
    blocks       = map (linesFromSrcSpan . H.ann) rec
    singleLine b = blockStart b == blockEnd b
