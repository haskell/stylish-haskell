#!/bin/sh
# Ported from https://github.com/ndmitchell/hlint/blob/master/misc/travis.sh

curl -sL https://raw.github.com/haskell/stylish-haskell/master/scripts/latest.sh | sh -s -- stylish-haskell $*
