stylish-haskell
===============

[![Build Status](https://secure.travis-ci.org/jaspervdj/stylish-haskell-imports.png?branch=master)](http://travis-ci.org/jaspervdj/stylish-haskell-imports)

Introduction
------------

A simple Haskell code prettifier

Features
--------

- Align and sort `import` statements
- Groups and wraps `{-# LANGUAGE #-}` pragmas (TODO)
- Removes trailing whitespace

Status
------

Doesn't yet work for multi-line imports, e.g.,

    import Control.Monad (forM,
        replicateM)
