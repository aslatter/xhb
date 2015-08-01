#!/usr/bin/env bash

OUT_DIR=cabalize
TEST_PROG=xcb-build-cabal

. shared.sh

OUT_FILE=${OUT_DIR}/cabalize.cabal

[ -f ${OUT_FILE} ] || {
    echo "Couldn't find generated cabal file: ${OUT_FILE}" 
    exit ${GENERAL_ERROR}
}

cp ${OUT_FILE} xhb.cabal

echo "Success!"
exit 0

