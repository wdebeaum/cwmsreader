#
# File: Makefile
# Creator: George Ferguson
# Created: Wed Jun 20 15:12:58 2012
# Time-stamp: <Tue Aug 25 16:54:54 CDT 2020 lgalescu>
#

NAME = trips-cwmsreader

include ../Makefile-common

# domain-specific terms for TextTagger
DOM_TERMS = domain-terms.tsv

SYS_DATA_DIR = $(etcdir)/cwmsreader

install:: install-data
	# src/Data/ usually makes this dir, but the git mirror lacks it, and
	# trips-cwms.sh fails without it, so we make it here too
	$(MKINSTALLDIRS) $(prefix)/etc/Data/

install-data:: ${CSM_DATA} ${DOM_TERMS} | ${SYS_DATA_DIR}
	$(INSTALL_DATA) $^ ${SYS_DATA_DIR}

${SYS_DATA_DIR}:
	mkdir -p $@

install-configs:: TextTagger-*.conf DrumGUI*.conf
	$(INSTALL_DATA) $^ $(etcdir)
