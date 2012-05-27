stylish-haskell
===============

[![Build Status](https://secure.travis-ci.org/jaspervdj/stylish-haskell-imports.png?branch=master)](http://travis-ci.org/jaspervdj/stylish-haskell-imports)

Introduction
------------

A simple Haskell code prettifier

Features
--------

- Aligns and sorts `import` statements
- Groups and wraps `{-# LANGUAGE #-}` pragmas (WIP)
- Removes trailing whitespace
- Replaces tabs by four spaces

VIM integration
---------------

Since it works as a filter it is pretty easy to integrate this with VIM.
Just call

    :%!stylish-haskell

or add a keybinding for it.
