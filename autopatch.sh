#!/usr/bin/env bash

. version.sh

[ -d patched ] && {

    diff -u -r generated/${XPROTO_VERSION} patched > patch
    rm -rf patched
    bash generate.sh
    diff -u -r generated/${XPROTO_VERSION} patched > patch
}
