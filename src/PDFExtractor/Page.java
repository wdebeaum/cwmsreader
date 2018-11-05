package TRIPS.PDFExtractor;

import java.io.IOException;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.pdmodel.common.PDRectangle;
import technology.tabula.ObjectExtractor;
//import technology.tabula.Page; conflicts (obvs.)
import technology.tabula.Rectangle;
import technology.tabula.detectors.NurminenDetectionAlgorithm;
import TRIPS.KQML.KQMLBadPerformativeException;
import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;
import TRIPS.KQML.KQMLPerformative;
import TRIPS.KQML.KQMLToken;
import TRIPS.util.cwc.Args;
import TRIPS.util.cwc.CWCException;
import TRIPS.util.cwc.InvalidArgument;

public class Page implements HasID {
  final String id;
  @Override public String getID() { return id; }
  final PDPage pdPage;
  final Document document;
  final int pageIndex;
  LinkedList<Region> regions;
  HashSet<Listener> listeners;

  public Page(PDPage pdPage, Document document, int pageIndex) {
    id = HasID.getNextIDAndPut(this);
    this.pdPage = pdPage;
    this.document = document;
    this.pageIndex = pageIndex;
    regions = new LinkedList<Region>();
    listeners = new HashSet<Listener>();
  }

  public PDRectangle getPDBBox() { return pdPage.getBBox(); }

  public PDPage getPDPage() { return pdPage; }

  public Document getDocument() { return document; }
  
  public int getPageIndex() { return pageIndex; }

  public List<Region> getRegions() { return regions; }

  /** Get the most recently added Region whose resize handle is at (x,y), or
   * null if there is no resize handle there.
   */
  public Region getRegionAt(int x, int y) {
    Iterator<Region> backwards = regions.descendingIterator();
    while (backwards.hasNext()) {
      Region r = backwards.next();
      if (r.isHighlighted() && !r.getHandleAt((double)x, (double)y).isEmpty())
	return r;
    }
    return null;
  }

  public void addRegion(Region region) {
    regions.add(region);
    emit(new Event(Event.Type.REGION_ADDED, region));
  }

  public void removeRegion(Region region) {
    regions.remove(region);
    emit(new Event(Event.Type.REGION_REMOVED, region));
  }

  /** Return an equivalent Tabula Page object. Note that Tabula "Page"s can be
   * either the whole page or an area of the page. This gets the former.
   */
  public technology.tabula.Page toTabulaPage() {
    return new ObjectExtractor(document.getPDDocument()).extract(pageIndex + 1);
  }

  public List<Region> detectTableRegions() throws IOException {
    // make a duplicate document containing a duplicate of this page, since
    // Tabula steals the page into a document of its own when you call detect()
    // (ARGH)
    PDDocument docCopy = new PDDocument();
    PDPage pdPageCopy = docCopy.importPage(pdPage);
    pdPageCopy.setRotation(pdPage.getRotation());
    technology.tabula.Page tabulaPage = new ObjectExtractor(docCopy).extract(1);
    // instead of just this:
    //technology.tabula.Page tabulaPage = toTabulaPage();
    // now we can call detect()
    List<Rectangle> rects = new NurminenDetectionAlgorithm().detect(tabulaPage);
    // and wrap the results in our Region objects
    List<Region> regs = new ArrayList<Region>(rects.size());
    for (Rectangle rect : rects) {
      // Tabula sometimes gives out-of-bounds rects (!)
      Rectangle clampedRect = Region.clampRectToPage(rect, this);
      regs.add(new Region(clampedRect, this));
    }
    docCopy.close(); // close the duplicate document so PDFBox doesn't complain
    return regs;
  }

  @Override public KQMLObject toKQML() {
    KQMLPerformative p = new KQMLPerformative("page");
    p.setParameter(":id", id);
    p.setParameter(":document", document.getID());
    p.setParameter(":index", Integer.toString(pageIndex));
    return p;
  }

  public static Page fromKQML(KQMLObject listOrID) throws CWCException, KQMLBadPerformativeException {
    if (listOrID instanceof KQMLList) {
      return fromKQML(new KQMLPerformative((KQMLList)listOrID));
    } else if (listOrID instanceof KQMLList) {
      KQMLPerformative perf = (KQMLPerformative)listOrID;
      KQMLToken idKQML =
	Args.getTypedArgument(perf, ":id", KQMLToken.class, new KQMLToken("nil"));
      String id = idKQML.toString().toLowerCase();
      if (id.equals("nil")) { // have no ID, get the page from the document
	KQMLObject docKQML =
	  Args.getTypedArgument(perf, ":document", KQMLObject.class);
	Document doc = Document.fromKQML(docKQML);
	int index = Args.getTypedArgument(perf, ":index", Integer.class);
	return doc.getPage(index);
      } else { // have ID, just get the existing object
        return HasID.get(id, Page.class);
      }
    } else if (listOrID instanceof KQMLToken) {
      return HasID.get(listOrID.toString(), Page.class);
    } else {
      throw new InvalidArgument("nil", ":page", "a list or id", listOrID);
    }
  }

  public static class Event {
    public static enum Type { REGION_ADDED, REGION_REMOVED, REGION_HIGHLIGHTED, REGION_UNHIGHLIGHTED };
    Type type;		public Type getType() { return type; }
    Region region;	public Region getRegion() { return region; }
    public Page getPage() { return region.getPage(); }
    public Event(Type type, Region region) {
      this.type = type;
      this.region = region;
    }
  }

  public interface Listener {
    void pageChanged(Event pageEvent);
  }
  
  public void addPageListener(Listener listener) {
    listeners.add(listener);
  }

  public void emit(Event event) {
    for (Listener l : listeners) {
      l.pageChanged(event);
    }
  }
}
