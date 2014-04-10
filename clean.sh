#!/usr/bin/env bash

pushd build-utils > /dev/null
cabal clean
cabal sandbox delete
popd > /dev/null

