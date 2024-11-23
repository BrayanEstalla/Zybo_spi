#
# This file is the sds-lib recipe.
#

SUMMARY = "cardano-lib"
SECTION = "PETALINUX/apps"
LICENSE = "MIT"
LIC_FILES_CHKSUM = "file://${COMMON_LICENSE_DIR}/MIT;md5=0835ade698e0bcf8506ecda2f7b4f302"

SRC_URI = "file://libcardano_api.so \
	"

S = "${WORKDIR}"

do_install() {
	     install -d ${D}/${libdir}
	     install -m 0755 ${S}/libcardano_api.so ${D}/${libdir}
}

FILES_${PN} += "${libdir}"
FILES_SOLIBSDEV = ""
