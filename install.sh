#!/bin/sh
#
# Script that is primarily written to be used on a jenkins CI. However, this
# should also work locally.
#

if [ ! -x "$HOME/.cabal/bin/cabal" ] ; then
    echo "Cabal with sandbox support (probably) not installed."
    return 1
fi

if [ ! -f "euler.cabal" ] ; then
    echo "Wrong working directory as MasterThesis.cabal cannot be found."
    return 2
fi

CABAL="$HOME/.cabal/bin/cabal"

if [ ! -f "./cabal.sandbox.config" ] ; then
    $CABAL sandbox init
fi

$CABAL install --only-dependencies --enable-tests \
    && $CABAL clean \
    && $CABAL configure --enable-tests \
    && $CABAL build \
    && $CABAL test
