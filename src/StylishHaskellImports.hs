module StylishHaskellImports
    ( stylishHaskellImports
    ) where


--------------------------------------------------------------------------------
import Data.Either (lefts, rights)
import Data.Function (on)
import Data.List (groupBy, sort)
import Control.Arrow ((|||))


--------------------------------------------------------------------------------
data Qualified = Unqualified | Qualified deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
stylishQualified :: Qualified -> String
stylishQualified Unqualified = "         "
stylishQualified Qualified   = "qualified"


--------------------------------------------------------------------------------
data Import = Import String Qualified String deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
importName :: Import -> String
importName (Import name _ _) = name


--------------------------------------------------------------------------------
parseImport :: String -> Maybe Import
parseImport str = case words str of
    ("import" : "qualified" : n : e) -> Just $ Import n Qualified   (unwords e)
    ("import" : n : e)               -> Just $ Import n Unqualified (unwords e)
    _                                -> Nothing


--------------------------------------------------------------------------------
stylishImports :: Int -> [Import] -> [String]
stylishImports _       [] = []
stylishImports longest is = map stylishImport $ sort is
  where
    pad str = str ++ replicate (longest - length str) ' '

    stylishImport (Import n q "") = unwords
        ["import", stylishQualified q, n]
    stylishImport (Import n q e)  = unwords
        ["import", stylishQualified q, pad n, e]


--------------------------------------------------------------------------------
stylishHaskellImports :: String -> String
stylishHaskellImports = unlines .
    concat .
    stylish .
    map rls .
    groupBy ((==) `on` isRight) .
    map (\i -> maybe (Left i) Right (parseImport i)) .
    lines
  where
    isRight (Right _) = True
    isRight _         = False

    stylish ls =
        let longest = maximum $ map (length . importName) $ concat $ rights ls
        in map (id ||| stylishImports longest) ls

    rls xs
        | not (null xs) && isRight (head xs) = Right (rights xs)
        | otherwise                          = Left (lefts xs)
