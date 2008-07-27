#! /usr/bin/env bash

. version.sh

[ -d working ] && {
    ./generate.sh
    diff -u generated/${XPROTO_VERSION} working > patch
}

