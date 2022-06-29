## stylish-haskell

<img src="./assets/Logo/SVG/RoundedLogo.svg" width="100px">

![Build Status](https://github.com/jaspervdj/stylish-haskell/workflows/CI/badge.svg)

## Introduction

A simple Haskell code prettifier. The goal is not to format all of the code in
a file, since I find those kind of tools often "get in the way". However,
manually cleaning up import statements etc. gets tedious very quickly.

This tool tries to help where necessary without getting in the way.

## Installation

You can install it using `stack install stylish-haskell` or `cabal install stylish-haskell`.

You can also install it using your package manager:

- Debian 9 or later: `apt-get install stylish-haskell`
- Ubuntu 16.10 or later: `apt-get install stylish-haskell`
- Arch Linux: `pacman -S stylish-haskell`

## Features

- Aligns and sorts `import` statements
- Groups and wraps `{-# LANGUAGE #-}` pragmas, can remove (some) redundant
  pragmas
- Removes trailing whitespace
- Aligns branches in `case` and fields in records
- Converts line endings (customizable)
- Replaces tabs by four spaces (turned off by default)
- Replaces some ASCII sequences by their Unicode equivalents (turned off by
  default)
- Format data constructors and fields in records.

Feature requests are welcome! Use the [issue tracker] for that.

[issue tracker]: https://github.com/haskell/stylish-haskell/issues

## Example

Turns:

```haskell
{-# LANGUAGE ViewPatterns, TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving,
            ViewPatterns,
    ScopedTypeVariables #-}

module Bad where

import Control.Applicative ((<$>))
import System.Directory (doesFileExist)

import qualified Data.Map as M
import      Data.Map    ((!), keys, Map)

data Point = Point { pointX, pointY :: Double , pointName :: String} deriving (Show)
```

into:

```haskell
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
```

## Configuration

The tool is customizable to some extent. It tries to find a config file in the
following order:

1. A file passed to the tool using the `-c/--config` argument
2. `.stylish-haskell.yaml` in the current directory (useful for per-directory
   settings)
3. `.stylish-haskell.yaml` in the nearest ancestor directory (useful for
   per-project settings)
4. `stylish-haskell/config.yaml` in the platform’s configuration directory
   (on Windows, it is %APPDATA%, elsewhere it defaults to `~/.config` and
   can be overridden by the `XDG_CONFIG_HOME` environment variable;
   useful for user-wide settings)
5. `.stylish-haskell.yaml` in your home directory (useful for user-wide
   settings)
6. The default settings.

Use `stylish-haskell --defaults > .stylish-haskell.yaml` to dump a
well-documented default configuration to a file, this way you can get started
quickly.

## Record formatting

Basically, stylish-haskell supports 4 different styles of records, controlled by `records`
in the config file.

Here's an example of all four styles:

```haskell
-- equals: "indent 2", "first_field": "indent 2"
data Foo a
  = Foo
      { a :: Int
      , a2 :: String
        -- ^ some haddock
      }
  | Bar
      { b :: a
      }
  deriving (Eq, Show)
  deriving (ToJSON) via Bar Foo

-- equals: "same_line", "first_field": "indent 2"
data Foo a = Foo
               { a :: Int
               , a2 :: String
                 -- ^ some haddock
               }
           | Bar
               { b :: a
               }
  deriving (Eq, Show)
  deriving (ToJSON) via Bar Foo

-- equals: "same_line", "first_field": "same_line"
data Foo a = Foo { a :: Int
                 , a2 :: String
                   -- ^ some haddock
                 }
           | Bar { b :: a
                 }
  deriving (Eq, Show)
  deriving (ToJSON) via Bar Foo

-- equals: "indent 2", first_field: "same_line"
data Foo a
  = Foo { a :: Int
        , a2 :: String
          -- ^ some haddock
        }
  | Bar { b :: a
        }
  deriving (Eq, Show)
  deriving (ToJSON) via Bar Foo
```

## Editor integration

### Haskell Language Server
[Haskell Language Server(HLS)][HLS] includes a [plugin][HLS stylish-haskell Plugin]
for stylish-haskell. By changing the formatting provider option
(`haskell.formattingProvider`) to `stylish-haskell` as described in
[HLS options][HLS option], any editors that support [Language Server Protocol][LSP]
can use stylish-haskell for formatting.

[HLS]: https://github.com/haskell/haskell-language-server
[HLS option]: https://haskell-language-server.readthedocs.io/en/latest/configuration.html#language-specific-server-options
[HLS stylish-haskell Plugin]: https://github.com/haskell/haskell-language-server/blob/master/plugins/hls-stylish-haskell-plugin/src/Ide/Plugin/StylishHaskell.hs
[LSP]: https://microsoft.github.io/language-server-protocol/

### VIM integration

Since it works as a filter it is pretty easy to integrate this with VIM.

You can call

    :%!stylish-haskell

and add a keybinding for it.

Or you can define `formatprg`

    :set formatprg=stylish-haskell

and then use `gq`.

Alternatively, [vim-autoformat] supports stylish-haskell. To have it
automatically reformat the files on save, add to your vimrc:

```vim
autocmd BufWrite *.hs :Autoformat
" Don't automatically indent on save, since vim's autoindent for haskell is buggy
autocmd FileType haskell let b:autoformat_autoindent=0
```

There are also plugins that run stylish-haskell automatically when you save a
Haskell file:

- [vim-stylish-haskell]
- [vim-stylishask]

[vim-stylish-haskell]: https://github.com/nbouscal/vim-stylish-haskell
[vim-stylishask]: https://github.com/alx741/vim-stylishask

### Emacs integration

[haskell-mode] for Emacs supports `stylish-haskell`. For configuration,
see [the “Using external formatters” section][haskell-mode/format] of the
haskell-mode manual.

[haskell-mode]: https://github.com/haskell/haskell-mode
[haskell-mode/format]: http://haskell.github.io/haskell-mode/manual/latest/Autoformating.html

### Atom integration

[ide-haskell] for Atom supports `stylish-haskell`.

[atom-beautify] for Atom supports Haskell using `stylish-haskell`.

[ide-haskell]: https://atom.io/packages/ide-haskell
[atom-beautify]: Https://atom.io/packages/atom-beautify

### Visual Studio Code integration

[stylish-haskell-vscode] for VSCode supports `stylish-haskell`.

[stylish-haskell-vscode]: https://github.com/vigoo/stylish-haskell-vscode

## Using with Continuous Integration

You can quickly grab the latest binary and run `stylish-haskell` like so:

    curl -sL https://raw.github.com/haskell/stylish-haskell/master/scripts/latest.sh | sh -s .

Where the `.` can be replaced with the arguments you pass to `stylish-haskell`.

## Credits

Written and maintained by Jasper Van der Jeugt.

Contributors:

- Chris Done
- Hiromi Ishii
- Leonid Onokhov
- Michael Snoyman
- Mikhail Glushenkov
- Beatrice Vergani
- Paweł Szulc
- Łukasz Gołębiewski
- Felix Mulder
