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
TEST_PROG=Scripts/Generate

. shared.sh

[ -f patch ] && {

    [ -d patched ] && rm -rf patched
    cp -r ${OUT_DIR} patched
    cd patched
    patch -u < ../patch || {
        cd ..
        echo "failed to patch properly"
        exit ${GENERAL_ERROR}
    }
    cd ..
}

echo "success!"
exit
