--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Step.UnicodeSyntax
    ( step
    ) where


--------------------------------------------------------------------------------
import           Data.List                                     (isPrefixOf,
                                                                sort)
import           Data.Map                                      (Map)
import qualified Data.Map                                      as M
import           Data.Maybe                                    (maybeToList)
import           GHC.Hs.Binds
import           GHC.Hs.Extension                              (GhcPs)
import           GHC.Hs.Types
--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Block
import           Language.Haskell.Stylish.Editor
import           Language.Haskell.Stylish.Module
import           Language.Haskell.Stylish.Step
import           Language.Haskell.Stylish.Step.LanguagePragmas (addLanguagePragma)
import           Language.Haskell.Stylish.Util

--------------------------------------------------------------------------------
unicodeReplacements :: Map String String
unicodeReplacements = M.fromList
    [ ("::", "∷")
    , ("=>", "⇒")
    , ("->", "→")
    , ("<-", "←")
    , ("forall", "∀")
    , ("-<", "↢")
    , (">-", "↣")
    ]


--------------------------------------------------------------------------------
replaceAll :: [(Int, [(Int, String)])] -> [Change String]
replaceAll = map changeLine'
  where
    changeLine' (r, ns) = changeLine r $ \str -> return $
        applyChanges
            [ change (Block c ec) (const repl)
            | (c, needle) <- sort ns
            , let ec = c + length needle - 1
            , repl <- maybeToList $ M.lookup needle unicodeReplacements
            ] str


--------------------------------------------------------------------------------
groupPerLine :: [((Int, Int), a)] -> [(Int, [(Int, a)])]
groupPerLine = M.toList . M.fromListWith (++) .
    map (\((r, c), x) -> (r, [(c, x)]))

-- | Find symbol positions in the module.  Currently only searches in type
-- signatures.
findSymbol :: Module -> Lines -> String -> [((Int, Int), String)]
findSymbol module' ls sym =
    [ (pos, sym)
    | TypeSig _ funLoc typeLoc <- everything (rawModuleDecls $ moduleDecls module') :: [Sig GhcPs]
    , (funStart, _)            <- infoPoints funLoc
    , (_, typeEnd)             <- infoPoints [hsSigWcType typeLoc]
    , pos                      <- maybeToList $ between funStart typeEnd sym ls
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
step :: Bool -> String -> Step
step = (makeStep "UnicodeSyntax" .) . step'


--------------------------------------------------------------------------------
step' :: Bool -> String -> Lines -> Module -> Lines
step' alp lg ls module' = applyChanges changes ls
  where
    changes = (if alp then addLanguagePragma lg "UnicodeSyntax" module' else []) ++
        replaceAll perLine
    toReplace = [ "::", "=>", "->" ]
    perLine = sort $ groupPerLine $ concatMap (findSymbol module' ls) toReplace
