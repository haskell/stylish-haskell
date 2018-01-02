#!/bin/sh
# Ported from https://raw.githubusercontent.com/ndmitchell/neil/master/misc/travis.sh

set -e

PACKAGE=stylish-haskell
echo Downloading and running $PACKAGE...

RELEASES=$(curl --silent https://github.com/jaspervdj/$PACKAGE/releases)
URL=https://github.com/$(echo $RELEASES | grep -o '\"[^\"]*-x86_64-linux\.tar\.gz\"' | sed s/\"//g | head -n1)
VERSION=$(echo $URL | sed -e 's/.*-\([\.0-9]\+\)-x86_64-linux\.tar\.gz/\1/')
TEMP=$(mktemp --directory .$PACKAGE-XXXXX)

cleanup(){
    rm -r $TEMP
}
trap cleanup EXIT

curl --progress-bar --location -o$TEMP/$PACKAGE.tar.gz $URL
tar -xzf $TEMP/$PACKAGE.tar.gz -C$TEMP
$TEMP/$PACKAGE-$VERSION/$PACKAGE $*
