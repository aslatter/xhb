#!/usr/bin/env bash

# Test the generating capabilities of the current state of
# XHB.  Creates something like a haskell module from
# xproto.xml.  Will download the XML files with curl
# if they can't be found.
#
# Thanks for testing out XHB!
#
# -Antoine
# aslatter@gmail.com

TEST_DIR=testTmp
TEST_PROG=GenerateTest
TEST_SOURCE=Examples/${TEST_PROG}.hs

DOWNLOAD_PATH=resources
XML_TARBALL=http://xcb.freedesktop.org/dist/xcb-proto-1.1.tar.gz
XML_DIR=${DOWNLOAD_PATH}/xcb-proto-1.1/src
XML_FILE=xproto.xml

XML_PATH=${XML_DIR}/${XML_FILE}

GENERAL_ERROR=1

#Does the XML file exist?
[ -f ${XML_DIR}/${XML_FILE} ] || {

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

[ -f ${XML_DIR}/${XML_FILE} ] || {
    echo "Soething went wrong, I can't find ${XML_FILE}"
    exit ${GENERAL_ERROR}
}

if [ -d ${TEST_DIR} ]; then
    rm -rf ${TEST_DIR}
fi
mkdir ${TEST_DIR}

ghc --make -o ${TEST_DIR}/${TEST_PROG} ${TEST_SOURCE} || {
    echo "failed to build ${TEST_PROG}"
    exit ${GENERAL_ERROR}
}

${TEST_DIR}/${TEST_PROG} ${XML_PATH} > ${TEST_DIR}/${XML_FILE}.out || {
    echo "failed to run ${TEST_PROG}"
    exit ${GENERAL_ERROR}
}

echo "Created file ${TEST_DIR}/${XML_FILE}.out"

exit
