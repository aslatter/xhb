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

DOWNLOAD_PATH=resources
XML_TARBALL=http://xcb.freedesktop.org/dist/xcb-proto-1.1.tar.gz
XML_DIR=${DOWNLOAD_PATH}/xcb-proto-1.1/src
XML_FILES=*.xml

GENERAL_ERROR=1

#Do we have runhaskell?
[ $(which runhaskell) ] || {
    echo "runhaskell not found!"
    exit ${GENERAL_ERROR}
}

#Do the XML files exist?
[ -d ${XML_DIR} ] || {

    [ $(which curl) ]  || {
	echo "Cannot find 'curl' in path"
	exit ${GENERAL_ERROR}
    }

    [ -d ${DOWNLOAD_PATH} ] || {
        #attempt to create the directory
	mkdir -p ${DOWNLOAD_PATH}
    }

    [ -d ${DOWNLOAD_PATH} ] || {
	#didn't create directory :-(
	echo "couldn't create directory ${DOWNLOAD_PATH}"
	exit ${GENERAL_ERROR}
    }

    curl ${XML_TARBALL} | tar -xzC ${DOWNLOAD_PATH}
}

[ -d ${XML_DIR} ] || {
    echo "Soething went wrong, I can't find ${XML_DIR}"
    exit ${GENERAL_ERROR}
}

#Cleanup after prvious run
if [ -d ${OUT_DIR} ]; then
    rm -rf ${OUT_DIR}
fi
mkdir ${OUT_DIR}

#Go!
runghc ${TEST_PROG} ${XML_DIR}/${XML_FILES} || {
    echo "failed!"
    exit ${GENERAL_ERROR}
}

[ -f patch ] && {

    [ -d patched ] && rm -rf patched
    cp -r generated patched
    cd patched
    patch -u < ../patch
    cd ..
}

echo "success!"
exit
