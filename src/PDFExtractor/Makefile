MAIN=PDFExtractor
PACKAGE=TRIPS.PDFExtractor
TABULA_VERSION=1.0.2
TABULA_JAR=tabula-$(TABULA_VERSION)-jar-with-dependencies.jar
USES=TRIPS.TripsModule TRIPS.KQML TRIPS.util TRIPS.util.cwc $(TABULA_JAR:.jar=)
JFLAGS=-cp $(TABULA_JAR):.

SRCS=$(MAIN).java \
	Document.java \
	HasID.java \
	MergedCell.java \
	Page.java \
	PDFPane.java \
	Region.java \
	Table.java

CLASSES=$(SRCS:.java=.class) \
	HasID$$NextID.class \
	Page$$Event.class \
	Page$$Event$$Type.class \
	Page$$Listener.class \
	PDFExtractor$$1.class \
	PDFExtractor$$TableRowSelectionListener.class \
	Region$$Coord.class \
	Table$$DeleteColumns.class \
	Table$$DeleteRows.class \
	Table$$Edit.class \
	Table$$MergeColumns.class \
	Table$$MergeRows.class \
	Table$$MergeTables.class \
	Table$$SelectColumns.class \
	Table$$SelectRows.class \
	Table$$SplitColumn.class

all:: $(TABULA_JAR)

install:: $(TABULA_JAR)

CONFIGDIR=../config
include $(CONFIGDIR)/java/prog.mk

$(TABULA_JAR):
	curl -L -O https://github.com/tabulapdf/tabula-java/releases/download/v$(TABULA_VERSION)/tabula-$(TABULA_VERSION)-jar-with-dependencies.jar

install::
	$(INSTALL_DATA) $(TABULA_JAR) $(etcdir)/java

distclean:: clean
	rm -f $(TABULA_JAR)
