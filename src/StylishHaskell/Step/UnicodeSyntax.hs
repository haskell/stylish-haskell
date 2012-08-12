--------------------------------------------------------------------------------
module StylishHaskell.Step.UnicodeSyntax
    ( step
    ) where


--------------------------------------------------------------------------------
import           Data.List                           (isPrefixOf, sort)
import           Data.Map                            (Map)
import qualified Data.Map                            as M
import           Data.Maybe                          (maybeToList)
import qualified Language.Haskell.Exts.Annotated     as H


--------------------------------------------------------------------------------
import           StylishHaskell.Block
import           StylishHaskell.Editor
import           StylishHaskell.Step
import           StylishHaskell.Step.LanguagePragmas (addLanguagePragma)
import           StylishHaskell.Util


--------------------------------------------------------------------------------
unicodeReplacements :: Map String String
unicodeReplacements = M.fromList
    [ ("::", "∷")
    , ("=>", "⇒")
    , ("->", "→")
    ]


--------------------------------------------------------------------------------
replaceAll :: [(Int, [(Int, String)])] -> [Change String]
replaceAll = map changeLine'
  where
    changeLine' (r, ns) = changeLine r $ \str -> return $
        flip applyChanges str
            [ change (Block c ec) (const repl)
            | (c, needle) <- sort ns
            , let ec = c + length needle - 1
            , repl <- maybeToList $ M.lookup needle unicodeReplacements
            ]


--------------------------------------------------------------------------------
groupPerLine :: [((Int, Int), a)] -> [(Int, [(Int, a)])]
groupPerLine = M.toList . M.fromListWith (++) .
    map (\((r, c), x) -> (r, [(c, x)]))


--------------------------------------------------------------------------------
typeSigs :: H.Module H.SrcSpanInfo -> Lines -> [((Int, Int), String)]
typeSigs module' ls =
    [ (pos, "::")
    | H.TypeSig loc _ _  <- everything module' :: [H.Decl H.SrcSpanInfo]
    , (start, end)       <- infoPoints loc
    , pos                <- maybeToList $ between start end "::" ls
    ]


--------------------------------------------------------------------------------
contexts :: H.Module H.SrcSpanInfo -> Lines -> [((Int, Int), String)]
contexts module' ls =
    [ (pos, "=>")
    | context      <- everything module' :: [H.Context H.SrcSpanInfo]
    , (start, end) <- infoPoints $ H.ann context
    , pos          <- maybeToList $ between start end "=>" ls
    ]


--------------------------------------------------------------------------------
typeFuns :: H.Module H.SrcSpanInfo -> Lines -> [((Int, Int), String)]
typeFuns module' ls =
    [ (pos, "->")
    | H.TyFun _ t1 t2 <- everything module'
    , let start = H.srcSpanEnd $ H.srcInfoSpan $ H.ann t1
    , let end   = H.srcSpanStart $ H.srcInfoSpan $ H.ann t2
    , pos <- maybeToList $ between start end "->" ls
    ]


--------------------------------------------------------------------------------
-- | Search for a needle in a haystack of lines. Only part the inside (startRow,
-- startCol), (endRow, endCol) is searched. The return value is the position of
-- the needle.
between :: (Int, Int) -> (Int, Int) -> String -> Lines -> Maybe (Int, Int)
between (startRow, startCol) (endRow, endCol) needle =
    search (startRow, startCol) .
    withLast (take endCol) .
    withHead (drop $ startCol - 1) .
    take (endRow - startRow + 1) .
    drop (startRow - 1)
  where
    search _      []            = Nothing
    search (r, _) ([] : xs)     = search (r + 1, 1) xs
    search (r, c) (x : xs)
        | needle `isPrefixOf` x = Just (r, c)
        | otherwise             = search (r, c + 1) (tail x : xs)


--------------------------------------------------------------------------------
step :: Bool -> Step
step = makeStep "UnicodeSyntax" . step'


--------------------------------------------------------------------------------
step' :: Bool -> Lines -> Module -> Lines
step' alp ls (module', _) = applyChanges changes ls
  where
    changes = (if alp then addLanguagePragma "UnicodeSyntax" module' else []) ++
        replaceAll perLine
    perLine = sort $ groupPerLine $
        typeSigs module' ls ++
        contexts module' ls ++
        typeFuns module' ls
