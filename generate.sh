#!/usr/bin/env bash

# Test the generating capabilities of the current state of
# XHB.  Creates something like haskell modules from
# the proto XML files.  Will download the XML files with
# curl if they can't be found.
#
# Thanks for testing out XHB!
#
# -Antoine
# aslatter@gmail.com

OUT_DIR=generated
TEST_PROG=xcb-build-generate

. shared.sh

# apply hacks to generated code

[ -f patch ] && {

    [ -d patched ] && rm -rf patched
    cp -r ${OUT_DIR} patched
    cd patched
    patch -u -p 1 < ../patch || {
        cd ..
        echo "failed to patch properly"
        exit ${GENERAL_ERROR}
    }
    cd ..
}

# rebuild SmokeTest.hs

[ -d patched ] && {

    [ -f SmokeTest.hs ] && rm -f SmokeTest.hs

    [ -f SmokeTest.hs ] || {

	echo "module SmokeTest where" > SmokeTest.hs
	cd patched
	find . | grep \.hs$ | sed -e "s|^\./||" -e  "s|\(.*\)\.hs|import \1|" -e "s|/|.|g" >> ../SmokeTest.hs
	cd ..
	echo "main = putStrLn \"Hello!\"" >> SmokeTest.hs
    }
}

echo "success!"
exit
