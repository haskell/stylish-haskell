stylish-haskell
===============

[![Build Status](https://secure.travis-ci.org/jaspervdj/stylish-haskell.png?branch=master)](http://travis-ci.org/jaspervdj/stylish-haskell)

Introduction
------------

A simple Haskell code prettifier. The goal is not to format all of the code in
a file, since I find those kind of tools often "get in the way".

Instead, this tool just tries to help where necessary.

Features
--------

- Aligns and sorts `import` statements
- Groups and wraps `{-# LANGUAGE #-}` pragmas
- Removes trailing whitespace
- Replaces tabs by four spaces

Feature requests are welcome! Use the [issue tracker] for that.

[issue tracker]: https://github.com/jaspervdj/stylish-haskell/issues

VIM integration
---------------

Since it works as a filter it is pretty easy to integrate this with VIM.
Just call

    :%!stylish-haskell

or add a keybinding for it.

Example
-------

Turns:

    -- | Some badly formatted Haskell code
    
    {-# LANGUAGE ViewPatterns, TemplateHaskell #-}
    {-# LANGUAGE GeneralizedNewtypeDeriving,
            ViewPatterns,
            ScopedTypeVariables #-}
    
    module Bad where
    
    import Control.Applicative ((<$>))
    import System.Directory (doesFileExist)
    
    import qualified Data.Map as M
    import      Data.Map    ((!), keys, Map)   

into:

    -- | Some badly formatted Haskell code
    
    {-# LANGUAGE GeneralizedNewtypeDeriving #-}
    {-# LANGUAGE ScopedTypeVariables        #-}
    {-# LANGUAGE TemplateHaskell            #-}
    {-# LANGUAGE ViewPatterns               #-}
    
    module Bad where
    
    import           Control.Applicative ((<$>))
    import           System.Directory    (doesFileExist)
    
    import           Data.Map            (Map, keys, (!))
    import qualified Data.Map            as M
