stylish-haskell-imports
=======================

[![Build Status](https://secure.travis-ci.org/jaspervdj/stylish-haskell-imports.png?branch=master)](http://travis-ci.org/jaspervdj/stylish-haskell-imports)

What
----

Align and sort imports in Haskell source code

Turns

    module Herp where

    import qualified Data.Map  as M
    import Control.Monad
    import       Data.Map     (Map)

    import Herp.Derp.Internals

    herp = putStrLn "import Hello world"

into

    module Herp where

    import           Control.Monad
    import           Data.Map            (Map)
    import qualified Data.Map            as M

    import           Herp.Derp.Internals

    herp = putStrLn "import Hello world"

Status
------

Doesn't yet work for multi-line imports, e.g.,

    import Control.Monad (forM,
        replicateM)
