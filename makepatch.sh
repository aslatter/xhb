#! /usr/bin/env bash

[ -d working ] && {
    ./generate.sh
    diff -u generated working > patch
}

