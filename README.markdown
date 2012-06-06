stylish-haskell
===============

[![Build Status](https://secure.travis-ci.org/jaspervdj/stylish-haskell.png?branch=master)](http://travis-ci.org/jaspervdj/stylish-haskell)

Introduction
------------

A simple Haskell code prettifier. The goal is not to format all of the code in
a file, since I find those kind of tools often "get in the way". However,
manually cleaning up import statements etc. gets tedious very quickly.

This tool tries to help where necessary without getting in the way.

You can install it using `cabal install stylish-haskell`.

Features
--------

- Aligns and sorts `import` statements
- Groups and wraps `{-# LANGUAGE #-}` pragmas, can remove (some) redundant
  pragmas
- Removes trailing whitespace
- Replaces tabs by four spaces (turned off by default)
- Replaces some ASCII sequences by their Unicode equivalents (turned off by
  default)

Feature requests are welcome! Use the [issue tracker] for that.

[issue tracker]: https://github.com/jaspervdj/stylish-haskell/issues

Example
-------

Turns:

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

    {-# LANGUAGE GeneralizedNewtypeDeriving #-}
    {-# LANGUAGE ScopedTypeVariables        #-}
    {-# LANGUAGE TemplateHaskell            #-}
    {-# LANGUAGE ViewPatterns               #-}
    
    module Bad where
    
    import           Control.Applicative ((<$>))
    import           System.Directory    (doesFileExist)
    
    import           Data.Map            (Map, keys, (!))
    import qualified Data.Map            as M

Configuration
-------------

The tool is customizable to some extent.

VIM integration
---------------

Since it works as a filter it is pretty easy to integrate this with VIM.
Just call

    :%!stylish-haskell

or add a keybinding for it.

Emacs integration
-----------------

[haskell-mode] for Emacs supports `stylish-haskell`. For configuration, see
[Emacs/Formatting] on the HaskellWiki.


[haskell-mode]: https://github.com/haskell/haskell-mode
[Emacs/Formatting]: http://www.haskell.org/haskellwiki/Emacs/Formatting
