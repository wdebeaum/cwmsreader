package TRIPS.PDFExtractor;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import javax.swing.JTable;
import javax.swing.table.AbstractTableModel;
import technology.tabula.RectangularTextContainer;
//import technology.tabula.Table; conflicts (obvs.)
import technology.tabula.extractors.BasicExtractionAlgorithm;
import TRIPS.KQML.*;
import TRIPS.util.cwc.Args;
import TRIPS.util.cwc.CWCException;
import TRIPS.util.cwc.InvalidArgument;
import TRIPS.util.cwc.UnknownAction;

/** Improvement on Tabula's Table class: it has an ID, can be the model for a
 * JTable, and can be edited.
 */
public class Table extends AbstractTableModel implements HasID {
  final String id;
  @Override public String getID() { return id; }

  // current state
  int numRows, numCols;
  List<List<RectangularTextContainer>> rows;
  // edit history
  /** The region that was originally selected for the whole table. */
  Region origin;
  /** The column boundaries actually used by Tabula. This includes both
   * automatic and manual boundaries between columns. Edits other than
   * SplitColumn don't affect this (e.g. MergeColumns doesn't delete entries).
   * Note that this list includes the boundary before the first column, because
   * it makes xCoordToColIndex() simpler, but Tabula doesn't actually receive
   * that one because then it would make an extra blank column at the
   * beginning.
   */
  List<Float> colBoundXs;
  /** The manually-added column boundaries only. */
  List<SplitColumn> splitColumns;
  /** All edits applied to this table, in order, except SplitColumn edits.
   * splitColumns apply before running Tabula.
   */
  List<Edit> history;

  public Table(technology.tabula.Table tabulaTable, Region origin) {
    this.origin = origin;
    setTabulaTable(tabulaTable);
    splitColumns = new LinkedList<SplitColumn>();
    history = new LinkedList<Edit>();
    this.id = HasID.getNextIDAndPut(this);
  }

  public Table(Region origin) {
    this(extractTabulaTable(origin), origin);
  }

  @Override public int getRowCount() { return numRows; }
  @Override public int getColumnCount() { return numCols; }
  @Override public Object getValueAt(int row, int column) {
    return textToHTML(getTextAt(row, column));
  }

  @Override public KQMLObject toKQML() {
    KQMLPerformative p = new KQMLPerformative("table");
    p.setParameter(":id", id);
    KQMLList rowsKQML = new KQMLList();
    for (List<RectangularTextContainer> row : rows) {
      KQMLList rowKQML = new KQMLList();
      for (RectangularTextContainer cell : row) {
	rowKQML.add(new KQMLString(getCellText(cell)));
      }
      rowsKQML.add(rowKQML);
    }
    p.setParameter(":data", rowsKQML);
    return p;
  }

  public void setTabulaTable(technology.tabula.Table tabulaTable) {
    numRows = tabulaTable.getRowCount();
    numCols = tabulaTable.getColCount();
    rows = tabulaTable.getRows();
    if (colBoundXs == null) {
      // fill colBoundXs with the minimum X coordinate of each column's cell
      // regions (among non-blank cells; blank cells have 0 width)
      colBoundXs = new ArrayList<Float>(numCols);
      // start with the maximum X coordinate for the page
      float minX = origin.getPage().getPDBBox().getUpperRightX();
      // iterate backwards over columns so that the colBoundXs we find are
      // monotonically non-increasing
      for (int j = numCols - 1; j >= 0; j--) {
	boolean blank = true;
	for (List<RectangularTextContainer> row : rows) {
	  RectangularTextContainer cell = row.get(j);
	  if (cell.getWidth() > 0) {
	    blank = false;
	    float x = (float)cell.getX();
	    if (x < minX) minX = x;
	  }
	}
	if (blank)
	  System.err.println("WARNING: Tabula gave us a column (" + j + ") with no non-empty cells");
	colBoundXs.add(minX - 1); // subtracting 1 makes it more stable
      }
      // reverse since we found them backwards
      Collections.reverse(colBoundXs);
    }
  }

  public static technology.tabula.Table extractTabulaTable(Region region, List<Float> colBoundXs) {
    BasicExtractionAlgorithm extractor = new BasicExtractionAlgorithm();
    List<technology.tabula.Table> tables =
      (colBoundXs == null ?
        extractor.extract(region.toTabulaPage()) :
        extractor.extract(region.toTabulaPage(), colBoundXs));
    // NOTE: basic extractor always returns exactly 1 table
    return tables.get(0);
  }

  public static technology.tabula.Table extractTabulaTable(Region region) {
    return extractTabulaTable(region, null);
  }

  public RectangularTextContainer getCellAt(int row, int column) {
    return rows.get(row).get(column);
  }

  public static String getCellText(RectangularTextContainer cell) {
    // NOTE: getText(true) seems nice, but it's implemented as "return null;" !
    return cell.getText();
  }

  public String getTextAt(int row, int column) {
    return getCellText(getCellAt(row, column));
  }

  /** Turn newlines into &lt;br&gt; elements so that JTable will understand
   * them.
  */
  public static String textToHTML(String text) {
    return "<html>" +
      text.
      replaceAll("&", "&amp;").
      replaceAll("<", "&lt;").
      replaceAll(">", "&gt;").
      replaceAll("\n", "<br>") +
    "</html>";
  }

  /** Set the height of each row in the JTable displaying this Table according
   * to the maximum number of lines in each cell of the row.
   */
  public void setRowHeights(JTable jt) {
    for (int i = 0; i < numRows; i++) {
      int maxNumLines = 1;
      for (int j = 0; j < numCols; j++) {
	String text = getTextAt(i, j);
	int numLines = text.split("\n").length;
	if (numLines > maxNumLines) maxNumLines = numLines;
      }
      jt.setRowHeight(i, maxNumLines * jt.getRowHeight());
    }
  }

  /** Find the index of the column after the given X coordinate. If all columns
   * are at least partly before the given X coordinate, return numCols.
   */
  public int xCoordToColIndex(float x) {
    // could do binary search, but linear is simpler to code
    int i = 0;
    for (; i < numCols; i++)
      if (colBoundXs.get(i) > x)
	break;
    return i;
  }

  //
  // editing steps
  //

  /** Apply the given edit and add it to the history. */
  public void edit(Edit ed) throws CWCException {
    if (ed instanceof SplitColumn) {
      splitColumns.add((SplitColumn)ed);
    } else {
      history.add(ed);
    }
    ed.apply();
    fireTableStructureChanged();
  }

  public abstract class Edit {
    /** Perform this editing operation on the parent Table. */
    public abstract void apply() throws CWCException;
    /** Adjust this edit to account for a new column being inserted at index
     * newCol (by SplitColumn).
     */
    public void insertColumn(int newCol) { }
    /** Return the new index of the same column after apply() has been called.
     * If the indexed column is now absent, return numCols.
     */
    public int applyToColIndex(int colIndex) { return colIndex; }
  }

  public Edit editFromKQML(KQMLPerformative perf) throws CWCException {
    String verb = perf.getVerb().toLowerCase();
    // ugh, java.
    if (verb.equals("merge-tables")) {
      return mergeTablesFromKQML(perf);
    } else if (verb.equals("delete-rows")) {
      return deleteRowsFromKQML(perf);
    } else if (verb.equals("delete-columns")) {
      return deleteColumnsFromKQML(perf);
    } else if (verb.equals("merge-rows")) {
      return mergeRowsFromKQML(perf);
    } else if (verb.equals("merge-columns")) {
      return mergeColumnsFromKQML(perf);
    } else if (verb.equals("select-and-reorder-rows")) {
      return selectRowsFromKQML(perf);
    } else if (verb.equals("select-and-reorder-columns")) {
      return selectColumnsFromKQML(perf);
    } else if (verb.equals("split-column")) {
      return splitColumnFromKQML(perf);
    } else {
      throw new UnknownAction(verb);
    }
  }

  /** Concatenate several tables vertically, this being the first. */
  public class MergeTables extends Edit {
    public final List<Table> others;
    public MergeTables(List<Table> others) { this.others = others; }
    @Override public void apply() throws CWCException {
      // make sure all tables have the same number of columns
      for (Table other : others) {
	if (other.numCols != numCols)
	  throw new InvalidArgument("merge-tables", ":other", "table with " + numCols + " columns", new KQMLToken(other.getID()));
      }
      for (Table other : others) {
	rows.addAll(other.rows);
	numRows += other.numRows;
      }
    }
  }
  public MergeTables mergeTablesFromKQML(KQMLPerformative perf) throws CWCException {
    KQMLList tableIDs = Args.getTypedArgument(perf, ":others", KQMLList.class);
    List<Table> tables = new ArrayList<Table>(tableIDs.size());
    for (KQMLObject tableID : tableIDs) {
      if (!(tableID instanceof KQMLToken))
	throw new InvalidArgument("merge-tables", ":other", "table ID", tableID);
      Table table = HasID.get(tableID.toString(), Table.class);
      tables.add(table);
    }
    return new MergeTables(tables);
  }

  /** Delete all rows from firstRow to lastRow. */
  public class DeleteRows extends Edit {
    public final int firstRow, lastRow;
    public DeleteRows(int firstRow, int lastRow) {
      this.firstRow = firstRow;
      this.lastRow = lastRow;
    }
    @Override public void apply() throws CWCException {
      rows.subList(firstRow, lastRow + 1).clear();
      numRows -= lastRow - firstRow + 1;
    }
  }
  public DeleteRows deleteRowsFromKQML(KQMLPerformative perf) throws CWCException {
    int first = Args.getTypedArgument(perf, ":first", Integer.class);
    int last = Args.getTypedArgument(perf, ":last", Integer.class);
    if (first < 0 || first >= numRows)
      throw new InvalidArgument(perf, ":first", "integer in [0," + numRows + ")");
    if (last < 0 || last >= numRows)
      throw new InvalidArgument(perf, ":last", "integer in [0," + numRows + ")");
    return new DeleteRows(first, last);
  }

  /** Delete all columns from firstCol to lastCol. */
  public class DeleteColumns extends Edit {
    public int firstCol, lastCol; // not final because SplitColumn might happen
    public DeleteColumns(int firstCol, int lastCol) {
      this.firstCol = firstCol;
      this.lastCol = lastCol;
    }
    @Override public void apply() throws CWCException {
      for (List<RectangularTextContainer> row : rows) {
	row.subList(firstCol, lastCol + 1).clear();
      }
      numCols -= lastCol - firstCol + 1;
    }
    @Override public void insertColumn(int newCol) {
      if (newCol <= firstCol) firstCol++;
      if (newCol <= lastCol) lastCol++;
    }
    @Override public int applyToColIndex(int colIndex) {
      if (colIndex >= firstCol && colIndex <= lastCol) { // we removed it
	return numCols;
      } else if (colIndex > lastCol) { // it was after what we removed
	return colIndex - (lastCol - firstCol + 1); // subtract number removed
      } else { // it was before what we removed
	return colIndex;
      }
    }
  }
  public DeleteColumns deleteColumnsFromKQML(KQMLPerformative perf) throws CWCException {
    int first = Args.getTypedArgument(perf, ":first", Integer.class);
    int last = Args.getTypedArgument(perf, ":last", Integer.class);
    if (first < 0 || first >= numCols)
      throw new InvalidArgument(perf, ":first", "integer in [0," + numCols + ")");
    if (last < 0 || last >= numCols)
      throw new InvalidArgument(perf, ":last", "integer in [0," + numCols + ")");
    return new DeleteColumns(first, last);
  }

  /** Turn all rows from firstRow to lastRow into a single row, with each cell
   * of the row being the column of cells joined with line breaks.
   */
  public class MergeRows extends Edit {
    public final int firstRow, lastRow;
    public MergeRows(int firstRow, int lastRow) {
      this.firstRow = firstRow;
      this.lastRow = lastRow;
    }
    @Override public void apply() throws CWCException {
      // replace first row with row of MergedCells
      List<RectangularTextContainer> newRow =
        new ArrayList<RectangularTextContainer>(numCols);
      for (int colIndex = 0; colIndex < numCols; colIndex++) {
	List<RectangularTextContainer> oldCells =
	  new ArrayList<RectangularTextContainer>(lastRow - firstRow + 1);
	for (int rowIndex = firstRow; rowIndex <= lastRow; rowIndex++) {
	  oldCells.add(getCellAt(rowIndex, colIndex));
	}
	newRow.add(MergedCell.fromRTCs(oldCells, false));
      }
      rows.set(firstRow, newRow);
      // remove all the non-first rows
      rows.subList(firstRow + 1, lastRow + 1).clear();
      numRows -= lastRow - firstRow;
    }
  }
  public MergeRows mergeRowsFromKQML(KQMLPerformative perf) throws CWCException {
    int first = Args.getTypedArgument(perf, ":first", Integer.class);
    int last = Args.getTypedArgument(perf, ":last", Integer.class);
    if (first < 0 || first >= numRows)
      throw new InvalidArgument(perf, ":first", "integer in [0," + numRows + ")");
    if (last < 0 || last >= numRows)
      throw new InvalidArgument(perf, ":last", "integer in [0," + numRows + ")");
    return new MergeRows(first, last);
  }

  /** Turn all columns from firstCol to lastCol into a single column, with each
   * cell of the column being the row of cells joined with spaces.
   */
  public class MergeColumns extends Edit {
    public int firstCol, lastCol; // not final because SplitColumn might happen
    public MergeColumns(int firstCol, int lastCol) {
      this.firstCol = firstCol;
      this.lastCol = lastCol;
    }
    @Override public void apply() throws CWCException {
      for (List<RectangularTextContainer> row : rows) {
	// replace first cell with a MergedCell
	List<RectangularTextContainer> oldCells =
	  row.subList(firstCol, lastCol + 1);
	row.set(firstCol, MergedCell.fromRTCs(oldCells, true));
	// remove all the non-first columns
	row.subList(firstCol + 1, lastCol + 1).clear();
      }
      numCols -= lastCol - firstCol;
    }
    @Override public void insertColumn(int newCol) {
      if (newCol <= firstCol) firstCol++;
      if (newCol <= lastCol) lastCol++;
    }
    @Override public int applyToColIndex(int colIndex) {
      if (colIndex >= firstCol && colIndex <= lastCol) { // we merged it
	return numCols;
      } else if (colIndex > lastCol) { // it was after what we merged
	return colIndex - (lastCol - firstCol); // subtract number merged away
      } else { // it was before what we merged
	return colIndex;
      }
    }
  }
  public MergeColumns mergeColumnsFromKQML(KQMLPerformative perf) throws CWCException {
    int first = Args.getTypedArgument(perf, ":first", Integer.class);
    int last = Args.getTypedArgument(perf, ":last", Integer.class);
    if (first < 0 || first >= numCols)
      throw new InvalidArgument(perf, ":first", "integer in [0," + numCols + ")");
    if (last < 0 || last >= numCols)
      throw new InvalidArgument(perf, ":last", "integer in [0," + numCols + ")");
    return new MergeColumns(first, last);
  }

  /** Select and reorder a subset of the rows of the table. */
  public class SelectRows extends Edit {
    public final List<Integer> selectedRows;
    public SelectRows(List<Integer> selectedRows) {
      this.selectedRows = selectedRows;
    }
    @Override public void apply() throws CWCException {
      List<List<RectangularTextContainer>> oldRows = rows;
      rows = new ArrayList<List<RectangularTextContainer>>(selectedRows.size());
      for (int rowIndex : selectedRows) {
	rows.add(oldRows.get(rowIndex));
      }
      numRows = selectedRows.size();
    }
  }
  public SelectRows selectRowsFromKQML(KQMLPerformative perf) throws CWCException {
    KQMLList rowIndicesKQML =
      Args.getTypedArgument(perf, ":rows", KQMLList.class);
    List<Integer> rowIndices = new ArrayList<Integer>(rowIndicesKQML.size());
    for (KQMLObject rowIndexKQML : rowIndicesKQML) {
      if (!(rowIndexKQML instanceof KQMLToken))
	throw new InvalidArgument("select-and-reorder-rows", ":row", "Integer", rowIndexKQML);
      int rowIndex = Integer.parseInt(rowIndexKQML.toString());
      if (rowIndex < 0 || rowIndex >= numRows)
	throw new InvalidArgument("select-and-reorder-rows", ":row", "integer in [0," + numRows + ")", rowIndexKQML);
      rowIndices.add(rowIndex);
    }
    return new SelectRows(rowIndices);
  }

  /** Select and reorder a subset of the columns of the table. */
  public class SelectColumns extends Edit {
    public final List<Integer> selectedCols;
    public SelectColumns(List<Integer> selectedCols) {
      this.selectedCols = selectedCols;
    }
    @Override public void apply() throws CWCException {
      for (int rowIndex = 0; rowIndex < numRows; rowIndex++) {
	List<RectangularTextContainer> oldRow = rows.get(rowIndex);
	List<RectangularTextContainer> newRow =
	  new ArrayList<RectangularTextContainer>(selectedCols.size());
	for (int colIndex : selectedCols) {
	  newRow.add(oldRow.get(colIndex));
	}
	rows.set(rowIndex, newRow);
      }
      numCols = selectedCols.size();
    }
    @Override public int applyToColIndex(int colIndex) {
      // TODO? check if colIndex is one of the selected columns? but it won't be in the only situation we call this method, when we've just added a SplitColumn edit.
      return selectedCols.size();
    }
  }
  public SelectColumns selectColumnsFromKQML(KQMLPerformative perf) throws CWCException {
    KQMLList colIndicesKQML =
      Args.getTypedArgument(perf, ":columns", KQMLList.class);
    List<Integer> colIndices = new ArrayList<Integer>(colIndicesKQML.size());
    for (KQMLObject colIndexKQML : colIndicesKQML) {
      if (!(colIndexKQML instanceof KQMLToken))
	throw new InvalidArgument("select-and-reorder-columns", ":column", "Integer", colIndexKQML);
      int colIndex = Integer.parseInt(colIndexKQML.toString());
      if (colIndex < 0 || colIndex >= numCols)
	throw new InvalidArgument("select-and-reorder-columns", ":column", "integer in [0," + numCols + ")", colIndexKQML);
      colIndices.add(colIndex);
    }
    return new SelectColumns(colIndices);
  }

  /** Split a column by adding a new column boundary at X coordinate
   * newColBoundX before running Tabula.
   */
  public class SplitColumn extends Edit {
    public final float newColBoundX;
    public SplitColumn(float newColBoundX) { this.newColBoundX = newColBoundX;}
    @Override public void apply() throws CWCException {
      // the index of the new column (to the right of the new boundary)
      int newColIndex = xCoordToColIndex(newColBoundX);
      // add the new boundary
      colBoundXs.add(newColIndex, newColBoundX);
      // rerun tabula (without first boundary because that would make a blank
      // column at the beginning)
      technology.tabula.Table tabulaTable =
	extractTabulaTable(origin, colBoundXs.subList(1, colBoundXs.size()));
      setTabulaTable(tabulaTable);
      // reapply the rest of history to the newly extracted table, after
      // adjusting for the new column, and adjusting the new column index after
      // each step
      for (Edit e : history) {
	e.insertColumn(newColIndex);
	e.apply();
	newColIndex = e.applyToColIndex(newColIndex);
      }
    }
  }
  public SplitColumn splitColumnFromKQML(KQMLPerformative perf) throws CWCException {
    // NOTE: Double because Args doesn't do Float
    double newColBoundX = Args.getTypedArgument(perf, ":at-x", Double.class);
    return new SplitColumn((float)newColBoundX);
  }

  /* TODO?
   * - split/merge individual cells
   *  > would have to basically reimplement JTable to display properly (maybe switch to JavaFX/TableView)
   * - add rows/columns from KQML instead of PDF
   * - change cell contents (again from KQML)
   * - sort rows with a Comparator
   * - filter rows with a test function (no standard java interface for that?)
   */
}
