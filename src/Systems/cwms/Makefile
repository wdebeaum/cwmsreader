#
# File: Makefile
# Creator: George Ferguson
# Created: Wed Jun 20 15:12:58 2012
# Time-stamp: <Tue Mar  6 10:15:52 CST 2018 lgalescu>
#

NAME = trips-cwms

include ../Makefile-common

CSM_DATA = events goals models goals-par

# domain-specific terms for TextTagger
DOM_TERMS = domain-terms.tsv

SYS_DATA_DIR = $(etcdir)/cwms

install:: install-data

install-data:: ${CSM_DATA} ${DOM_TERMS} | ${SYS_DATA_DIR}
	$(INSTALL_DATA) $^ ${SYS_DATA_DIR}

${SYS_DATA_DIR}:
	mkdir -p $@

install-configs:: TextTagger-*.conf DrumGUI*.conf
	$(INSTALL_DATA) $^ $(etcdir)
