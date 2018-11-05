package TRIPS.PDFExtractor;

import java.io.File;
import java.io.IOException;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.swing.event.MouseInputListener;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import TRIPS.KQML.*;
import TRIPS.util.cwc.Args;
import TRIPS.util.cwc.CWCException;
import TRIPS.util.cwc.InvalidArgument;
import TRIPS.util.cwc.StandardCWCModule;
import TRIPS.util.cwc.SwingWindowManager;
import TRIPS.util.cwc.UnknownAction;
import TRIPS.util.cwc.WindowConfig;

/**
 * PDFExtractor - semi-automatically extracts table data from PDFs
 */
public class PDFExtractor extends StandardCWCModule implements KeyListener, MouseInputListener, MouseWheelListener {
  Region currentRegion;
  EnumSet<Region.Coord> currentHandle;
  boolean currentRegionIsNew;
  Map<File, PDFPane> pdfFile2pane;
  Map<Table, JTable> tableModel2view;

  public PDFExtractor(String[] argv) {
    super(argv);
    pdfFile2pane = new HashMap<File, PDFPane>();
    tableModel2view = new HashMap<Table, JTable>();
  }

  @Override
  public void init() {
    name = "PDFExtractor";
    super.init();
    windowManager = new SwingWindowManager(this);
    ready();
  }

  @Override
  public void subscribe() throws IOException {
    super.subscribe();
    send(KQMLPerformative.fromString(
      "(subscribe :content (request &key :content (display . *)))"));
    send(KQMLPerformative.fromString(
      "(subscribe :content (request &key :content (display-table . *)))"));
    send(KQMLPerformative.fromString(
      "(subscribe :content (request &key :content (parse-table . *)))"));
    send(KQMLPerformative.fromString(
      "(subscribe :content (request &key :content (edit-table . *)))"));
    send(KQMLPerformative.fromString(
      "(subscribe :content (request &key :content (detect-table-regions . *)))"));
    send(KQMLPerformative.fromString(
      "(subscribe :content (request &key :content (select . *)))"));
    send(KQMLPerformative.fromString(
      "(subscribe :content (request &key :content (deselect . *)))"));
  }

  @Override
  public void restart() {
    super.restart();
    HasID.clear();
  }

  @Override
  public void declareCapabilities() throws IOException {
    send(KQMLPerformative.fromString(
      "(tell :content (define-service :name display :component " + name +
      " :input (" +
        "(input" + 
        " :name file" +
	" :gloss \"PDF file to display\"" +
	" :id-code FILE" +
	" :format ont::file" +
	" :requirements :required) " +
	"(input" +
	" :name page" +
	" :gloss \"index of page in file to display\"" +
	" :id-code PAGE" +
	" :format number" +
	" :requirements :optional)" +
        ")" +
      " :output nil))"));
    send(KQMLPerformative.fromString(
      "(tell :content (define-service :name display-table :component " + name +
      " :input (" +
        "(input" + 
        " :name table" +
	" :gloss \"ID of table to display\"" +
	" :id-code TABLE" +
	" :format ont::symbol" +
	" :requirements :required)" +
        ")" +
      " :output nil))"));
    send(KQMLPerformative.fromString(
      "(tell :content (define-service :name parse-table :component " + name +
      " :input (" +
	"(input" +
	" :name region" +
	" :gloss \"PDF page region to parse as a table\"" +
	" :id-code REGION" +
	" :format ont::symbol" +
	" :requirements :required)" +
        ")" +
      " :output (" +
        "(output" +
	" :name table" +
	" :gloss \"parsed table structure\"" +
	" :id-code DATA" +
	" :format ont::list" +
        ")" +
      ")))"));
    send(KQMLPerformative.fromString(
      "(tell :content (define-service :name edit-table :component " + name +
      " :input (" +
	"(input" +
	" :name table" +
	" :gloss \"ID of table to modify\"" +
	" :id-code TABLE" +
	" :format ont::symbol" +
	" :requirements :required) " +
	"(input" +
	" :name edit" +
	" :gloss \"description of the edit to make\"" +
	" :id-code EDIT" +
	" :format ont::list" +
	" :requirements :required)" +
        ")" +
      " :output (" +
        "(output" +
	" :name table" +
	" :gloss \"edited table structure\"" +
	" :id-code DATA" +
	" :format ont::list" +
        ")" +
      ")))"));
    send(KQMLPerformative.fromString(
      "(tell :content (define-service :name detect-table-regions :component " + name +
      " :input (" +
	"(input" +
	" :name page" +
	" :gloss \"page to detect table regions in\"" +
	" :id-code PAGE" +
	" :format (or ont::list ont::symbol)" +
	" :requirements :required)" +
        ")" +
      " :output (" +
        "(output" +
	" :name regions" +
	" :gloss \"detected table region structures\"" +
	" :id-code REGION" +
	" :format ont::list" +
        ")" +
      ")))"));
    send(KQMLPerformative.fromString(
      "(tell :content (define-service :name select :component " + name +
      " :input (" +
        "(input" + 
        " :name what" +
	" :gloss \"thing to select\"" +
	" :id-code THING" +
	" :format ont::list" +
	" :requirements :required)" +
        ")" +
      " :output nil))"));
    send(KQMLPerformative.fromString(
      "(tell :content (define-service :name deselect :component " + name +
      " :input (" +
        "(input" + 
        " :name what" +
	" :gloss \"thing to deselect\"" +
	" :id-code THING" +
	" :format ont::list" +
	" :requirements :required)" +
        ")" +
      " :output nil))"));
  }

  @Override
  public boolean receiveRequest(KQMLPerformative msg, String verb, KQMLPerformative content) throws CWCException, IOException {
    if (verb.equals("detect-table-regions")) {
      KQMLObject pageKQML = Args.getTypedArgument(content, ":page", KQMLObject.class);
      Page page = Page.fromKQML(pageKQML);
      List<Region> regions = page.detectTableRegions();
      KQMLList regionsKQML = new KQMLList();
      for (Region region : regions) {
	regionsKQML.add(region.toKQML());
      }
      KQMLPerformative reply = new KQMLPerformative("reply");
      KQMLPerformative report = new KQMLPerformative("report");
      KQMLPerformative answer = new KQMLPerformative("answer");
      answer.setParameter(":regions", regionsKQML);
      report.setParameter(":content", answer);
      reply.setParameter(":content", report);
      reply(msg, reply);
    } else if (verb.equals("display")) {
      KQMLPerformative file = Args.getTypedArgument(content, ":file", KQMLPerformative.class);
      int page = Args.getTypedArgument(content, ":page", Integer.class, 0);
      String filename = Args.getTypedArgument(file, ":name", String.class);
      displayPDF(new File(filename), page, new WindowConfig(content));
    } else if (verb.equals("display-table")) {
      KQMLToken tableID = Args.getTypedArgument(content, ":table", KQMLToken.class);
      Table table = HasID.get(tableID.toString(), Table.class);
      displayTable(table, new WindowConfig(content));
    } else if (verb.equals("parse-table")) {
      KQMLToken regionID = Args.getTypedArgument(content, ":region", KQMLToken.class);
      Region region = HasID.get(regionID.toString(), Region.class);
      parseTable(msg, region);
    } else if (verb.equals("edit-table")) {
      KQMLToken tableID = Args.getTypedArgument(content, ":table", KQMLToken.class);
      Table table = HasID.get(tableID.toString(), Table.class);
      KQMLPerformative editKQML = Args.getTypedArgument(content, ":edit", KQMLPerformative.class);
      Table.Edit edit = table.editFromKQML(editKQML);
      table.edit(edit);
      table.setRowHeights(tableModel2view.get(table));
      KQMLPerformative reply = new KQMLPerformative("reply");
      KQMLPerformative report = new KQMLPerformative("report");
      KQMLPerformative answer = new KQMLPerformative("answer");
      answer.setParameter(":table", table.toKQML());
      report.setParameter(":content", answer);
      reply.setParameter(":content", report);
      reply(msg, reply);
    } else if (verb.equals("select") || verb.equals("deselect")) {
      boolean isSelect = verb.equals("select");
      KQMLPerformative what = Args.getTypedArgument(content, ":what", KQMLPerformative.class);
      String whatVerb = what.getVerb().toLowerCase();
      if (whatVerb.equals("row")) {
	KQMLToken tableID = Args.getTypedArgument(what, ":table", KQMLToken.class);
	Table table = HasID.get(tableID.toString(), Table.class);
	int index = Args.getTypedArgument(what, ":index", Integer.class);
	selectRow(table, index, isSelect);
      } else if (whatVerb.equals("rectangle")) {
	Region region = Region.fromKQML(what);
	region.setHighlighted(isSelect);
	regionChanged(region, isSelect ? "selected" : "deselected");
      } else {
	throw new InvalidArgument(content, ":what", "row or rectangle");
      }
    } else if (verb.equals("describe")) {
      // extend WindowManager's describe request to cover HasID objects
      String id =
        Args.getTypedArgument(content, ":what", KQMLToken.class).
	toString().toLowerCase();
      if (HasID.has(id)) {
	HasID o = HasID.get(id, HasID.class);
	KQMLPerformative reply = new KQMLPerformative("reply");
	KQMLPerformative report = new KQMLPerformative("report");
	KQMLPerformative answer = new KQMLPerformative("answer");
	answer.setParameter(":description", o.toKQML());
	report.setParameter(":content", answer);
	reply.setParameter(":content", report);
	reply(msg, reply);
      } else {
	return super.receiveRequest(msg, verb, content);
      }
    } else {
      return super.receiveRequest(msg, verb, content);
    }
    return true;
  }

  /** Read a PDF document from the file and display the indexed page in a new
   * window. If we're already displaying this file, just set the page and
   * window config.
  */
  void displayPDF(File file, int page, WindowConfig wc) throws IOException {
    file = file.getCanonicalFile();
    if (pdfFile2pane.containsKey(file)) {
      // old file/pane/window; just set page and window config
      PDFPane pane = pdfFile2pane.get(file);
      if (pane.isDisplayable()) { // window still open
	pane.setPage(page);
	JFrame window = (JFrame)pane.getTopLevelAncestor();
	pageDisplayed(pane.getPage(), window);
	((SwingWindowManager)windowManager).configureWindow(window, wc);
	return;
      } // else window was closed, fall through to make new window again
    }
    // new file/pane/window
    if (wc.title == null) {
      wc.title = file.getPath() + " - " + name;
    }
    PDFPane pane = new PDFPane(file, page);
    pdfFile2pane.put(file, pane);
    pane.addKeyListener(this);
    pane.addMouseListener(this);
    pane.addMouseMotionListener(this);
    pane.addMouseWheelListener(this);
    //JFrame window = new JFrame();
    Map.Entry<KQMLToken,JFrame> entry = windowManager.createWindow(wc);
    JFrame window = entry.getValue();
    // call pageDisplayed() in a WindowListener so it comes after "opened"
    window.addWindowListener(new WindowAdapter() {
      @Override public void windowOpened(WindowEvent evt) {
	pageDisplayed(pane.getPage(), window);
	evt.getWindow().removeWindowListener(this);
      }
    });
    window.setBackground(Color.WHITE);
    window.add(pane);
    window.pack();
    window.setVisible(true);
  }

  /** Display a previously-parsed table in a new window. */
  void displayTable(Table model, WindowConfig wc) {
    if (wc.title == null) {
      wc.title = model.getID() + " - " + name;
    }
    JTable view = new JTable(model);
    view.getTableHeader().setReorderingAllowed(false);
    view.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    view.getSelectionModel().addListSelectionListener(new TableRowSelectionListener(model));
    view.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
    model.setRowHeights(view);
    JScrollPane scroll =
      new JScrollPane(view,
                      ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS,
                      ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS);
    Map.Entry<KQMLToken,JFrame> entry = windowManager.createWindow(wc);
    JFrame window = entry.getValue();
    window.add(scroll);
    window.pack(); // FIXME this doesn't actually fit the window to the size of the table (scroll getting in the way?)
    window.setVisible(true);
    tableModel2view.put(model, view);
  }

  /** Parse a table in the given region and reply to msg with the data. */
  void parseTable(KQMLPerformative msg, Region region) {
    Table table = new Table(region);
    // construct reply
    KQMLPerformative reply = new KQMLPerformative("reply");
    KQMLPerformative report = new KQMLPerformative("report");
    KQMLPerformative answer = new KQMLPerformative("answer");
    KQMLObject tableKQML = table.toKQML();
    answer.setParameter(":table", tableKQML);
    report.setParameter(":content", answer);
    reply.setParameter(":content", report);
    reply(msg, reply);
  }

  void selectRow(Table model, int index, boolean isSelect) {
    JTable view = tableModel2view.get(model);
    ListSelectionModel selModel = view.getSelectionModel();
    if (isSelect) {
      selModel.setSelectionInterval(index, index);
    } else {
      selModel.removeSelectionInterval(index, index);
    }
    rowSelected(model, index, isSelect);
  }

  void pageDisplayed(Page page, JFrame window) {
    KQMLPerformative tell = new KQMLPerformative("tell");
    KQMLPerformative report = new KQMLPerformative("report");
    KQMLPerformative action = new KQMLPerformative("displayed");
    action.setParameter(":what", page.toKQML());
    action.setParameter(":where", ((SwingWindowManager)windowManager).getID(window));
    report.setParameter(":content", action);
    tell.setParameter(":content", report);
    send(tell);
  }

  /** Report that a region was (de)selected. */
  void regionChanged(Region region, String verb) {
    KQMLPerformative tell = new KQMLPerformative("tell");
    KQMLPerformative report = new KQMLPerformative("report");
    KQMLPerformative action = new KQMLPerformative(verb);
    action.setParameter(":what", region.toKQML());
    report.setParameter(":content", action);
    tell.setParameter(":content", report);
    send(tell);
  }

  /** Report that a table row was (de)selected. */
  void rowSelected(Table table, int index, boolean isSelect) {
    KQMLPerformative tell = new KQMLPerformative("tell");
    KQMLPerformative report = new KQMLPerformative("report");
    KQMLPerformative action = new KQMLPerformative(isSelect ? "selected" : "deselected");
    KQMLPerformative row = new KQMLPerformative("row");
    row.setParameter(":table", table.getID());
    row.setParameter(":index", Integer.toString(index));
    action.setParameter(":what", row);
    report.setParameter(":content", action);
    tell.setParameter(":content", report);
    send(tell);
  }

  //// *Listener methods ////
  
  @Override public void keyPressed(KeyEvent evt) {
    Component component = evt.getComponent();
    if (component instanceof PDFPane) {
      PDFPane pane = (PDFPane)component;
      int pageSet = 0;
      int pageInc = 0;
      switch (evt.getKeyCode()) {
	case KeyEvent.VK_PAGE_UP:
	  pageInc = -1;
	  break;
	case KeyEvent.VK_PAGE_DOWN:
	  pageInc = 1;
	  break;
	case KeyEvent.VK_HOME:
	  pageSet = 0;
	  break;
	case KeyEvent.VK_END:
	  pageSet = pane.getNumPages() - 1;
	  break;
	default:
	  return;
      }
      if (pageInc != 0) {
	pane.incPage(pageInc);
      } else {
	pane.setPage(pageSet);
      }
      // TODO re-pack containing JFrame for different-size pages?
      pageDisplayed(pane.getPage(), (JFrame)pane.getTopLevelAncestor());
    }
  }

  @Override public void keyReleased(KeyEvent evt) {/* do nothing */}
  @Override public void keyTyped(KeyEvent evt) {/* do nothing */}

  @Override public void mouseClicked(MouseEvent evt) {/* do nothing */}
  @Override public void mouseEntered(MouseEvent evt) {/* do nothing */}
  @Override public void mouseExited(MouseEvent evt) {/* do nothing */}

  @Override public void mousePressed(MouseEvent evt) {
    Component component = evt.getComponent();
    if (component instanceof PDFPane) {
      PDFPane pane = (PDFPane)component;
      if (evt.getButton() == MouseEvent.BUTTON1) {
	int x = evt.getX(), y = evt.getY();
	Page page = pane.getPage();
	currentRegion = page.getRegionAt(x,y);
	if (currentRegion == null) { // make a new region
	  currentRegion = new Region(x,y, x,y, page);
	  currentHandle = EnumSet.of(Region.Coord.X2, Region.Coord.Y2);
	  currentRegionIsNew = true;
	} else { // resize an existing region
	  currentHandle = currentRegion.getHandleAt(x, y);
	  currentRegionIsNew = false;
	}
      }
    }
  }

  @Override public void mouseReleased(MouseEvent evt) {
    Component component = evt.getComponent();
    if (component instanceof PDFPane && evt.getButton() == MouseEvent.BUTTON1 &&
        currentRegion != null) {
      if (currentRegion.getAbsWidth() > 0 && currentRegion.getAbsHeight() > 0) {
	currentRegion.normalize(); // for my own sanity
	regionChanged(currentRegion, (currentRegionIsNew ? "selected" : "changed"));
      } else { // degenerate selection, forget it
	if (!currentRegionIsNew)
	  regionChanged(currentRegion, "deselected");
	currentRegion.remove();
      }
      currentRegion = null;
    }
  }

  @Override public void mouseDragged(MouseEvent evt) {
    if (currentRegion != null) {
      currentRegion.setCoords(currentHandle, evt.getX(), evt.getY());
      evt.getComponent().repaint();
    }
  }

  @Override public void mouseMoved(MouseEvent evt) {
    Component component = evt.getComponent();
    if (component instanceof PDFPane) {
      PDFPane pane = (PDFPane)component;
      pane.setCursor(pane.getCursorAt(evt.getX(), evt.getY()));
    }
  }

  @Override public void mouseWheelMoved(MouseWheelEvent evt) {
    Component component = evt.getComponent();
    if (component instanceof PDFPane) {
      PDFPane pane = (PDFPane)component;
      int numClicks = evt.getWheelRotation();
      pane.incPage(numClicks);
      // TODO re-pack containing JFrame for different-size pages?
      pageDisplayed(pane.getPage(), (JFrame)pane.getTopLevelAncestor());
    }
  }

  /** Annoyingly, ListSelectionEvent doesn't give us a way to find out which
   * table it came from, so we have to make a dedicated object to be a listener
   * that remembers which table it's for.
   */
  class TableRowSelectionListener implements ListSelectionListener {
    Table table;
    public TableRowSelectionListener(Table table) {
      this.table = table;
    }

    @Override public void valueChanged(ListSelectionEvent evt) {
      ListSelectionModel model = (ListSelectionModel)evt.getSource();
      if ((!model.isSelectionEmpty()) && (!evt.getValueIsAdjusting())) {
	rowSelected(table, model.getLeadSelectionIndex(), true);
	// TODO detect user deselecting a row?
      }
    }
  }
  
  public static void main(String[] argv) {
    // this is supposed to help PDFBox render faster on Java 8
    System.setProperty("sun.java2d.cmm", "sun.java2d.cmm.kcms.KcmsServiceProvider");
    // this is supposed to help PDFBox>=2.0.4 render faster
    //System.setProperty("org.apache.pdfbox.rendering.UsePureJavaCMYKConversion", "true");
    // but it seems to have the opposite effect!
    new PDFExtractor(argv).run();
  }
}

