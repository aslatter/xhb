#!/usr/bin/env bash

[ -d patched ] || {
    echo "Could not find generated files!"
    echo "Maybe they haven't been generated yet?"
    echo
    echo "Failed!"
    exit 1
}

cd patched
MODULES=$(find . | grep \.hs$ | sed -e "s|^\./||" -e "s|\(.*\)\.hs|\1|" -e "s|/|.|g")
cd ..

runghc Scripts/Cabal Templates/cabal.template xhb.cabal patched ${MODULES} || {
    echo "Failed!"
    exit 1
}

echo "Success!"
exit 0

