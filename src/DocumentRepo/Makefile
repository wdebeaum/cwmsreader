MAIN=DocumentRepo
PACKAGE=TRIPS.DocumentRepo
USES=TRIPS.TripsModule TRIPS.KQML TRIPS.util TRIPS.util.cwc

SRCS= \
	DB.java \
	DocumentList.java \
	DocumentRepo.java \
	StreamForward.java


CLASSES=$(SRCS:.java=.class) \
	DB$$Listener.class \
	DocumentList$$1.class \
	DocumentList$$DetailsMouseListener.class \
	DocumentList$$DLTransferHandler.class \
	DocumentList$$DLTransferable.class

CONFIGDIR=../config
include $(CONFIGDIR)/java/prog.mk

INSTALL_DIR=$(etcdir)/$(MAIN)

all:: repo.db

install:: repo.db
	$(MKINSTALLDIRS) $(INSTALL_DIR)
	# only install repo.db if it isn't already installed, so we don't lose
	# data added after a previous install
	if test ! -e $(INSTALL_DIR)/repo.db ; then \
	  $(INSTALL_DATA) repo.db $(INSTALL_DIR) ; \
	fi

repo.db: schema.sqlite
	sqlite3 $@ <$<

clean::
	rm -f repo.db
