#!/usr/bin/env bash

. version.sh
. protodefs.sh

[ -d ${XML_DIR} ] && {
    PATCHFILE=${PWD}/xmlpatch
    pushd ${DOWNLOAD_PATH}/${XPROTO_PACKAGE} > /dev/null
    diff -u -r src patched > ${PATCHFILE}
    popd > /dev/null
}
