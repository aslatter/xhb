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

OUT_DIR=parsed
TEST_PROG=Scripts/Parse

. shared.sh

echo "success!"
exit
