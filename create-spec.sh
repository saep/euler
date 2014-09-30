#!/bin/sh

if [ ! -f euler.cabal ] ; then
    echo "Probably not in the right directory!"
    echo "Please cd to the project root (i.e. where euler.cabal is located."
fi

if [ -z "$1" ] ; then
    echo "No problem number given. Usage: create-spec.sh 042"
    exit 1
fi

if cabal install ; then
    :
else
    echo "cabal install failed, cannot create test case"
    exit 1
fi

f="executable/Euler/P""$1"".hs"
if [ ! -f "$f" ] ; then
    echo "$f does not exist"
    exit 1
fi

t="test-suite/Euler/P""$1""Spec.hs"
if [ ! -f "$t" ] ; then
    mkdir -pv `dirname "$t"`
    cat > "$t" <<DELIM
module Euler.P$1Spec
    ( spec ) where

import Euler.P$1

import Test.Hspec

spec :: Spec
spec = describe "Solution for Problem $1" $ do
           it "returns" $ do
               solve \`shouldReturn\` `./.cabal-sandbox/bin/euler -q -p $1`
DELIM
fi


