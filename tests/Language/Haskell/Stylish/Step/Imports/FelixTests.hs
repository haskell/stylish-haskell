--------------------------------------------------------------------------------
-- | Tests contributed by Felix Mulder as part of
-- <https://github.com/haskell/stylish-haskell/pull/293>.
{-# LANGUAGE OverloadedLists #-}
module Language.Haskell.Stylish.Step.Imports.FelixTests
  ( tests
  ) where


--------------------------------------------------------------------------------
import           Prelude                               hiding (lines)
import           Test.Framework                        (Test, testGroup)
import           Test.Framework.Providers.HUnit        (testCase)
import           Test.HUnit                            (Assertion)


--------------------------------------------------------------------------------
import           Language.Haskell.Stylish.Step.Imports
import           Language.Haskell.Stylish.Tests.Util   (assertSnippet)


--------------------------------------------------------------------------------
tests :: Test
tests = testGroup "Language.Haskell.Stylish.Step.Imports.FelixTests"
    [ testCase "Hello world" ex0
    , testCase "Sorted simple" ex1
    , testCase "Sorted import lists" ex2
    , testCase "Sorted import lists and import decls" ex3
    , testCase "Import constructor all" ex4
    , testCase "Import constructor specific" ex5
    , testCase "Import constructor specific sorted" ex6
    , testCase "Imports step does not change rest of file" ex7
    , testCase "Imports respect groups" ex8
    , testCase "Imports respects whitespace between groups" ex9
    , testCase "Doesn't add extra space after 'hiding'" ex10
    , testCase "Should be able to format symbolic imports" ex11
    , testCase "Able to merge equivalent imports" ex12
    , testCase "Obeys max columns setting" ex13
    , testCase "Obeys max columns setting with two in each" ex14
    , testCase "Respects multiple groups" ex15
    , testCase "Doesn't delete nullary imports" ex16
    ]


--------------------------------------------------------------------------------
ex0 :: Assertion
ex0 = assertSnippet (step Nothing felixOptions)
    [ "import B"
    , "import A"
    ]
    [ "import           A"
    , "import           B"
    ]

ex1 :: Assertion
ex1 = assertSnippet (step Nothing felixOptions)
    [ "import B"
    , "import A"
    , "import C"
    , "import qualified A"
    , "import qualified B as X"
    ]
    [ "import           A"
    , "import qualified A"
    , "import           B"
    , "import qualified B as X"
    , "import           C"
    ]

ex2 :: Assertion
ex2 = assertSnippet (step Nothing felixOptions)
    [ "import B"
    , "import A (X)"
    , "import C"
    , "import qualified A as Y (Y)"
    ]
    [ "import           A (X)"
    , "import qualified A as Y (Y)"
    , "import           B"
    , "import           C"
    ]

ex3 :: Assertion
ex3 = assertSnippet (step Nothing felixOptions)
    [ "import B"
    , "import A (X, Z, Y)"
    , "import C"
    , "import qualified A as A0 (b, Y, a)"
    , "import qualified D as D0 (Y, b, a)"
    , "import qualified E as E0 (b, a, Y)"
    ]
    [ "import           A (X, Y, Z)"
    , "import qualified A as A0 (Y, a, b)"
    , "import           B"
    , "import           C"
    , "import qualified D as D0 (Y, a, b)"
    , "import qualified E as E0 (Y, a, b)"
    ]

ex4 :: Assertion
ex4 = assertSnippet (step Nothing felixOptions)
    [ "import A (X, Z(..), Y)"
    ]
    [ "import           A (X, Y, Z (..))"
    ]

ex5 :: Assertion
ex5 = assertSnippet (step Nothing felixOptions)
    [ "import A (X, Z(Z), Y)"
    ]
    [ "import           A (X, Y, Z (Z))"
    ]

ex6 :: Assertion
ex6 = assertSnippet (step Nothing felixOptions)
    [ "import A (X, Z(X, Z, Y), Y)"
    ]
    [ "import           A (X, Y, Z (X, Y, Z))"
    ]

ex7 :: Assertion
ex7 = assertSnippet (step Nothing felixOptions)
    [ "module Foo (tests) where"
    , "import B"
    , "import A (X, Z, Y)"
    , "import C"
    , "import qualified A as A0 (b, Y, a)"
    , "import qualified D as D0 (Y, b, a)"
    , "import qualified E as E0 (b, a, Y)"
    , "-- hello"
    , "foo :: Int"
    , "foo = 1"
    ]
    [ "module Foo (tests) where"
    , "import           A (X, Y, Z)"
    , "import qualified A as A0 (Y, a, b)"
    , "import           B"
    , "import           C"
    , "import qualified D as D0 (Y, a, b)"
    , "import qualified E as E0 (Y, a, b)"
    , "-- hello"
    , "foo :: Int"
    , "foo = 1"
    ]

ex8 :: Assertion
ex8 = assertSnippet (step Nothing felixOptions)
    [ "import B"
    , "-- Group divisor"
    , "import A (X)"
    , "import C"
    , "import qualified  A as Y (Y)"
    ]
    [ "import           B"
    , "-- Group divisor"
    , "import           A (X)"
    , "import qualified A as Y (Y)"
    , "import           C"
    ]

ex9 :: Assertion
ex9 = assertSnippet (step Nothing felixOptions)
    [ "--------"
    , "import B"
    , ""
    , "-- Group divisor"
    , "import A (X)"
    , "import C"
    , "import qualified A as Y (Y)"
    ]
    [ "--------"
    , "import           B"
    , ""
    , "-- Group divisor"
    , "import           A (X)"
    , "import qualified A as Y (Y)"
    , "import           C"
    ]

ex10 :: Assertion
ex10 = assertSnippet (step Nothing felixOptions)
    [ "import B         hiding      (X)"
    , "import A  hiding (X)"
    ]
    [ "import           A hiding (X)"
    , "import           B hiding (X)"
    ]

ex11 :: Assertion
ex11 = assertSnippet (step Nothing felixOptions)
    [ "import Data.Aeson ((.=))"
    , "import A  hiding (X)"
    ]
    [ "import           A          hiding (X)"
    , "import           Data.Aeson ((.=))"
    ]

ex12 :: Assertion
ex12 = assertSnippet (step Nothing felixOptions)
    [ "import Data.Aeson ((.=))"
    , "import Data.Aeson ((.=))"
    , "import A  hiding (X)"
    ]
    [ "import           A          hiding (X)"
    , "import           Data.Aeson ((.=))"
    ]

ex13 :: Assertion
ex13 = assertSnippet (step (Just 10) felixOptions)
    [ "import Foo (A, B, C, D)"
    , "import A  hiding (X)"
    ]
    [ "import           A   hiding (X)"
    , "import           Foo (A)"
    , "import           Foo (B)"
    , "import           Foo (C)"
    , "import           Foo (D)"
    ]

ex14 :: Assertion
ex14 = assertSnippet (step (Just 27) felixOptions)
    [ "import Foo (A, B, C, D)"
    , "import A  hiding (X)"
    ]
    [ "import           A   hiding (X)"
    , "import           Foo (A, B)"
    , "import           Foo (C, D)"
    ]

ex15 :: Assertion
ex15 = assertSnippet (step (Just 100) felixOptions)
    [ "module Custom.Prelude"
    , "  ( LazyByteString"
    , "  , UUID"
    , "  , decodeUtf8Lenient"
    , "  , error"
    , "  , headMay"
    , "  , module X"
    , "  , nextRandomUUID"
    , "  , onChars"
    , "  , proxyOf"
    , "  , show"
    , "  , showStr"
    , "  , toLazyByteString"
    , "  , toStrictByteString"
    , "  , type (~>)"
    , "  , uuidToText"
    , "  ) where"
    , ""
    , "--------------------------------------------------------------------------------"
    , "import Prelude as X hiding ((!!), appendFile, error, foldl, head, putStrLn, readFile, show, tail, take, unlines, unwords, words, writeFile)"
    , "import qualified Prelude"
    , ""
    , "--------------------------------------------------------------------------------"
    , "import Control.Lens as X ((%~), (&), (.~), (?~), (^.), (^?), _Left, _Right, iat, over, preview, sans, set, to, view)"
    , "import Control.Lens.Extras as X (is)"
    , ""
    , "--------------------------------------------------------------------------------"
    , "import Control.Applicative as X ((<|>))"
    , "import Control.Monad as X ((<=<), (>=>), guard, unless, when)"
    , "import Control.Monad.Except as X (ExceptT (..), MonadError (..), liftEither, runExceptT, withExceptT)"
    , "import Control.Monad.IO.Unlift as X"
    , "import Control.Monad.Reader as X (MonadReader (..), ReaderT (..), asks)"
    , "import Control.Monad.Trans.Class as X (MonadTrans (lift))"
    , "--------------------------------------------------------------------------------"
    ]
    [ "module Custom.Prelude"
    , "  ( LazyByteString"
    , "  , UUID"
    , "  , decodeUtf8Lenient"
    , "  , error"
    , "  , headMay"
    , "  , module X"
    , "  , nextRandomUUID"
    , "  , onChars"
    , "  , proxyOf"
    , "  , show"
    , "  , showStr"
    , "  , toLazyByteString"
    , "  , toStrictByteString"
    , "  , type (~>)"
    , "  , uuidToText"
    , "  ) where"
    , ""
    , "--------------------------------------------------------------------------------"
    , "import           Prelude                   as X hiding (appendFile, error, foldl, head, putStrLn, readFile, show, tail, take, unlines, unwords, words, writeFile, (!!))"
    , "import qualified Prelude"
    , ""
    , "--------------------------------------------------------------------------------"
    , "import           Control.Lens              as X (_Left, _Right, iat, over, preview, sans, set, to)"
    , "import           Control.Lens              as X (view, (%~), (&), (.~), (?~), (^.), (^?))"
    , "import           Control.Lens.Extras       as X (is)"
    , ""
    , "--------------------------------------------------------------------------------"
    , "import           Control.Applicative       as X ((<|>))"
    , "import           Control.Monad             as X (guard, unless, when, (<=<), (>=>))"
    , "import           Control.Monad.Except      as X (ExceptT (..), MonadError (..), liftEither)"
    , "import           Control.Monad.Except      as X (runExceptT, withExceptT)"
    , "import           Control.Monad.IO.Unlift   as X"
    , "import           Control.Monad.Reader      as X (MonadReader (..), ReaderT (..), asks)"
    , "import           Control.Monad.Trans.Class as X (MonadTrans (lift))"
    , "--------------------------------------------------------------------------------"
    ]

ex16 :: Assertion
ex16 = assertSnippet (step Nothing felixOptions)
    [ "module Foo where"
    , ""
    , "import           B ()"
    , "import           A ()"
    ]
    [ "module Foo where"
    , ""
    , "import           A ()"
    , "import           B ()"
    ]

felixOptions :: Options
felixOptions = defaultOptions
    { listAlign = Repeat
    }
