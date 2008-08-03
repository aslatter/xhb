#! /usr/bin/env bash

. version.sh

[ -d working ] && {
    diff -u -r generated/${XPROTO_VERSION} working > patch
}

