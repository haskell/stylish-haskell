#!/bin/sh
# Ported from https://raw.githubusercontent.com/ndmitchell/neil/master/misc/travis.sh

set -e

PACKAGE=stylish-haskell
echo Downloading and running $PACKAGE...

RELEASES=$(curl --silent https://github.com/haskell/$PACKAGE/releases)
URL=https://github.com/$(echo $RELEASES | grep -o '\"[^\"]*-linux-x86_64\.tar\.gz\"' | sed s/\"//g | head -n1)
VERSION=$(echo $URL | sed -e 's/.*-\(v[\.0-9]\+-linux-x86_64\)\.tar\.gz/\1/')
TEMP=$(mktemp --directory .$PACKAGE-XXXXX)

cleanup(){
    rm -r $TEMP
}
trap cleanup EXIT

curl --progress-bar --location -o$TEMP/$PACKAGE.tar.gz $URL
tar -xzf $TEMP/$PACKAGE.tar.gz -C$TEMP
$TEMP/$PACKAGE-$VERSION/$PACKAGE $*
