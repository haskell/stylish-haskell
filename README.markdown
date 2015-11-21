stylish-haskell
===============

[![Build Status](https://secure.travis-ci.org/jaspervdj/stylish-haskell.svg?branch=master)](http://travis-ci.org/jaspervdj/stylish-haskell)

Introduction
------------

A simple Haskell code prettifier. The goal is not to format all of the code in
a file, since I find those kind of tools often "get in the way". However,
manually cleaning up import statements etc. gets tedious very quickly.

This tool tries to help where necessary without getting in the way.

Installation
------------

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

    data Point = Point
        { pointX, pointY :: Double
        , pointName :: String
        } deriving (Show)

into:

    {-# LANGUAGE GeneralizedNewtypeDeriving #-}
    {-# LANGUAGE ScopedTypeVariables        #-}
    {-# LANGUAGE TemplateHaskell            #-}

    module Bad where

    import           Control.Applicative ((<$>))
    import           System.Directory    (doesFileExist)

    import           Data.Map            (Map, keys, (!))
    import qualified Data.Map            as M

    data Point = Point
        { pointX, pointY :: Double
        , pointName      :: String
        } deriving (Show)

Configuration
-------------

The tool is customizable to some extent. It tries to find a config file in the
following order:

1. A file passed to the tool using the `-c/--config` argument
2. `.stylish-haskell.yaml` in the current directory (useful for per-directory
   settings)
3. `.stylish-haskell.yaml` in the nearest ancestor directory (useful for
   per-project settings)
4. `.stylish-haskell.yaml` in your home directory (useful for user-wide
   settings)
5. The default settings.

Use `stylish-haskell --defaults > .stylish-haskell.yaml` to dump a
well-documented default configuration to a file, this way you can get started
quickly.

VIM integration
---------------

Since it works as a filter it is pretty easy to integrate this with VIM.
Just call

    :%!stylish-haskell

or add a keybinding for it.

There is also the [vim-stylish-haskell] plugin, which runs stylish-haskell
automatically when you save a Haskell file.

[vim-stylish-haskell]: https://github.com/nbouscal/vim-stylish-haskell

Emacs integration
-----------------

[haskell-mode] for Emacs supports `stylish-haskell`. For configuration, see
[Emacs/Formatting] on the HaskellWiki.

[haskell-mode]: https://github.com/haskell/haskell-mode
[Emacs/Formatting]: http://wiki.haskell.org/Emacs/Formatting

Atom integration
----------------

[ide-haskell] for Atom supports `stylish-haskell`.

[ide-haskell]: https://atom.io/packages/ide-haskell

Credits
-------

Written and maintained by Jasper Van der Jeugt.

Contributors:

- Chris Done
- Hiromi Ishii
- Leonid Onokhov
- Michael Snoyman
- Mikhail Glushenkov

Changelog
---------

- `0.5.9.0`
    * Add `compact_line` setting for Language Pragma styling
