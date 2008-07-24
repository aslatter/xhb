
XPROTO_VERSION=xcb-proto-1.1

DOWNLOAD_PATH=resources
XML_TARBALL=http://xcb.freedesktop.org/dist/${XPROTO_VERSION}.tar.gz
XML_DIR=${DOWNLOAD_PATH}/${XPROTO_VERSION}/src
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
