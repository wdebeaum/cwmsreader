MAIN=PDFExtractor
PACKAGE=TRIPS.PDFExtractor
TABULA_VERSION=1.0.2
TABULA_JAR=tabula-$(TABULA_VERSION)-jar-with-dependencies.jar
USES=TRIPS.TripsModule TRIPS.KQML TRIPS.util TRIPS.util.cwc $(TABULA_JAR:.jar=)
JFLAGS=-cp $(CLASSPATH):$(TABULA_JAR):. -Xlint:unchecked

SRCS=$(MAIN).java \
	ActionWithButton.java \
	Cell.java \
	CellProperties.java \
	Document.java \
	DocumentMenu.java \
	EditedCell.java \
	HasID.java \
	HasSortOrders.java \
	HTMLBuilder.java \
	IntervalRelation.java \
	LearningGUI.java \
	MergedCell.java \
	Page.java \
	PDFPane.java \
	Region.java \
	ResultSet.java \
	SpanTable.java \
	SyntheticCell.java \
	Table.java \
	TableEditMenu.java \
	TableSelection.java \
	TableSelectionListener.java \
	TableSelectionModel.java \
	TextChunker.java \
	TextMatch.java \
	VisibilityFilter.java

CLASSES=$(SRCS:.java=.class) \
	CellProperties$$1.class \
	CellProperties$$Editor.class \
	CellProperties$$Editor$$Verifier.class \
	CellProperties$$Type.class \
	DocumentMenu$$OpenAction.class \
	DocumentMenu$$DetectTableAction.class \
	DocumentMenu$$ParseTableAction.class \
	HasID$$NextID.class \
	LearningGUI$$1.class \
	LearningGUI$$2.class \
	LearningGUI$$3.class \
	LearningGUI$$3$$1.class \
	LearningGUI$$4.class \
	LearningGUI$$5.class \
	LearningGUI$$6.class \
	LearningGUI$$7.class \
	LearningGUI$$8.class \
	LearningGUI$$9.class \
	LearningGUI$$10.class \
	LearningGUI$$11.class \
	LearningGUI$$AnswerReceiver.class \
	LearningGUI$$Promise.class \
	LearningGUI$$RequestSender.class \
	LearningGUI$$Rule.class \
	Page$$Event.class \
	Page$$Event$$Type.class \
	Page$$Listener.class \
	Page$$Paragraph.class \
	Page$$ParagraphLineComparator.class \
	PDFExtractor$$1.class \
	PDFExtractor$$2.class \
	PDFExtractor$$3.class \
	PDFExtractor$$4.class \
	PDFPane$$Listener.class \
	Region$$1.class \
	Region$$Coord.class \
	Region$$NamedColor.class \
	Region$$Order.class \
	Region$$Order$$Key.class \
	Region$$Order$$SimpleKey.class \
	Region$$Order$$DistanceKey.class \
	Region$$Order$$ColorKey.class \
	Region$$Relation.class \
	Region$$Source.class \
	Table$$1.class \
	Table$$2.class \
	Table$$3.class \
	Table$$AutoSplitColumn.class \
	Table$$CellCoordinateComparator.class \
	Table$$BadEdit.class \
	Table$$DeleteColumns.class \
	Table$$DeleteRows.class \
	Table$$Edit.class \
	Table$$EditCell.class \
	Table$$EditCell$$1.class \
	Table$$EditCells.class \
	Table$$EditCells$$1.class \
	Table$$MergeCells.class \
	Table$$MergeColumns.class \
	Table$$MergeRows.class \
	Table$$MergeTables.class \
	Table$$SelectColumns.class \
	Table$$SelectRows.class \
	Table$$SplitColumn.class \
	TableEditMenu$$AutoMultiEditAction.class \
	TableEditMenu$$AutoSplitColumnsAction.class \
	TableEditMenu$$AutoMergeCellsAction.class \
	TableEditMenu$$EditAction.class \
	TableEditMenu$$MergeTablesAction.class \
	TableEditMenu$$SaveAction.class \
	TableEditMenu$$SelectionEditAction.class \
	TableEditMenu$$SplitColumnAction.class \
	TableEditMenu$$UndoRedoAction.class \
	TextChunker$$1.class \
	TextMatch$$1.class \
	TextMatch$$Searchable.class \
	TextMatch$$Order.class \
	TextMatch$$Order$$Key.class \
	VisibilityFilter$$Renderer.class \
	VisibilityFilter$$ShownGlyph.class

all:: $(TABULA_JAR)

install:: $(TABULA_JAR)

CONFIGDIR=../config
include $(CONFIGDIR)/java/prog.mk

$(TABULA_JAR): technology/tabula/PublicRSI.java
	curl -L -O https://github.com/tabulapdf/tabula-java/releases/download/v$(TABULA_VERSION)/tabula-$(TABULA_VERSION)-jar-with-dependencies.jar
	$(JAVAC) $(JFLAGS) technology/tabula/PublicRSI.java
	$(JAR) -uf $(TABULA_JAR) technology/tabula/PublicRSI.class

install::
	$(INSTALL_DATA) $(TABULA_JAR) $(etcdir)/java

# override the version of this rule from ../config/java/common.mk to add JFLAGS
# (to get the jar file in the classpath) and -Xdoclint:none (to get rid of
# annoying warnings about missing @param docs)
doc/index.html: $(SRCS)
	test -d doc || mkdir doc
	$(JAVADOC) $(JFLAGS) -Xdoclint:none -d doc -private $(PACKAGE)

distclean:: clean
	rm -f $(TABULA_JAR)
