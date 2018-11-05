package TRIPS.PDFExtractor;

import java.util.List;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.Map;
import java.util.HashMap;
import java.awt.Color;
import java.awt.Graphics;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.pdmodel.common.PDRectangle;
//import technology.tabula.Page; conflicts
import technology.tabula.Rectangle;
import TRIPS.KQML.KQMLBadPerformativeException;
import TRIPS.KQML.KQMLObject;
import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLPerformative;
import TRIPS.KQML.KQMLString;
import TRIPS.KQML.KQMLToken;
import TRIPS.util.cwc.Args;
import TRIPS.util.cwc.CWCException;
import TRIPS.util.cwc.InvalidArgument;
import TRIPS.util.cwc.UnknownObject;

/** Represents a rectangular region of a page in a PDF document. */
public class Region implements HasID {
  final String id;
  @Override public String getID() { return id; }
  Rectangle rect;
  Page page;
  Color color;
  boolean highlighted;

  public static enum Coord { X1, Y1, X2, Y2 };

  //accessors
  public double getX1() { return rect.getX(); }
  public double getY1() { return rect.getY(); }
  public double getX2() { return rect.getX() + rect.getWidth() - 1.0; }
  public double getY2() { return rect.getY() + rect.getHeight() - 1.0; }
  // NOTE: rect.get{Min,Max}{X,Y}() don't account for negative dimensions
  public double getMinX() { return Math.min(getX1(), getX2()); }
  public double getMinY() { return Math.min(getY1(), getY2()); }
  public double getMaxX() { return Math.max(getX1(), getX2()); }
  public double getMaxY() { return Math.max(getY1(), getY2()); }
  public double getAbsWidth() { return Math.abs(rect.getWidth()); }
  public double getAbsHeight() { return Math.abs(rect.getHeight()); }
  public double getSignedWidth() { return rect.getWidth(); }
  public double getSignedHeight() { return rect.getHeight(); }
  public Page getPage() { return page; }
  public Color getColor() { return color; }
  public void setColor(Color c) { color = c; }

  /** Rearrange coordinates to get rid of any negative width or height. */
  public void normalize() {
    rect.setRect(getMinX(), getMinY(), getAbsWidth(), getAbsHeight());
  }
  
  /** Set X2 and Y2. */
  public void setXY2(double x, double y) {
    // clamp x,y to be within page
    PDRectangle pageBB = page.getPDBBox();
    x = Math.max(pageBB.getLowerLeftX(), Math.min(pageBB.getUpperRightX(), x));
    y = Math.max(pageBB.getLowerLeftY(), Math.min(pageBB.getUpperRightY(), y));
    // set width,height based on x,y and existing x1,y1
    double x1 = rect.getX(), y1 = rect.getY();
    rect.setRect(x1, y1, x - x1 + 1.0, y - y1 + 1.0);
  }

  /** Set the coordinates of one of the four corners, specified by coords,
   * which should contain one X and one Y.
   */
  public void setCoords(EnumSet<Coord> coords, double x, double y) {
    double x1 = coords.contains(Coord.X1) ? x : getX1(),
	   y1 = coords.contains(Coord.Y1) ? y : getY1(),
	   x2 = coords.contains(Coord.X2) ? x : getX2(),
	   y2 = coords.contains(Coord.Y2) ? y : getY2();
    rect.setRect(clampRectToPage(x1, y1, x2, y2, page));
  }

  final double HANDLE_RADIUS = 5; // pixels

  /** Get the coords to pass to setCoords if we start dragging from (x,y). */
  public EnumSet<Coord> getHandleAt(double x, double y) {
    EnumSet<Coord> ret = EnumSet.noneOf(Coord.class);
    if (rect.intersects(x - HANDLE_RADIUS, y - HANDLE_RADIUS,
			HANDLE_RADIUS * 2 + 1, HANDLE_RADIUS * 2 + 1)) {
      if (Math.abs(x - getX1()) <= HANDLE_RADIUS) ret.add(Coord.X1);
      if (Math.abs(y - getY1()) <= HANDLE_RADIUS) ret.add(Coord.Y1);
      if (Math.abs(x - getX2()) <= HANDLE_RADIUS) ret.add(Coord.X2);
      if (Math.abs(y - getY2()) <= HANDLE_RADIUS) ret.add(Coord.Y2);
    }
    return ret;
  }

  public Region(Rectangle rect, Page page, Color color, String id) {
    this.rect = rect;
    this.page = page;
    this.color = color;
    if (this.color == null) {
      this.color = Region.generateRandomColor();
    }
    if (id == null) {
      this.id = HasID.getNextIDAndPut(this);
    } else {
      this.id = id.toLowerCase();
      HasID.put(this);
    }
    highlighted = true;
    this.page.addRegion(this);
  }

  public Region(Rectangle rect, Page page) {
    this(rect, page, null, null);
  }

  /** Return the minimum Rectangle containing both (x1,y1) and (x2,y2), except
   * clamped to be within the given page.
   */
  public static Rectangle clampRectToPage(
      double x1, double y1, double x2, double y2, Page page) {
    PDRectangle pageBB = page.getPDBBox();
    float px1 = pageBB.getLowerLeftX(), py1 = pageBB.getLowerLeftY(),
          px2 = pageBB.getUpperRightX(), py2 = pageBB.getUpperRightY();
    x1 = Math.max(px1, Math.min(px2, x1));
    y1 = Math.max(py1, Math.min(py2, y1));
    x2 = Math.max(px1, Math.min(px2, x2));
    y2 = Math.max(py1, Math.min(py2, y2));
    return new Rectangle(
      // NOTE: Tabula's Rectangle class insanely swaps the x1/y1 (left/top)
      // parameters in the constructor. Why?!?!
      (float)y1, (float)x1,
      (float)(x2 - x1 + 1.0), (float)(y2 - y1 + 1.0)
    );
  }

  public static Rectangle clampRectToPage(Rectangle r, Page page) {
    double x1 = r.getX(), y1 = r.getY();
    return clampRectToPage(x1, y1, x1 + r.getWidth(), y1 + r.getHeight(), page);
  }

  /** Create an axis-aligned rectangle containing pixel positions (x1,y1) and (x2,y2) on the PDF page page, to be drawn in the specified color, having the specified ID.
   */
  public Region(double x1, double y1, double x2, double y2, 
                Page page, Color color, String id) {
    this(clampRectToPage(x1,y1,x2,y2,page), page, color, id);
  }
  
  /** Create a Region, assigning it a random-hued color and an arbitrary ID. */
  public Region(double x1, double y1, double x2, double y2, Page page) {
    this(x1,y1, x2,y2, page, null, null);
  }

  public void remove() {
    page.removeRegion(this);
    HasID.remove(this);
  }

  /** Generate a color for a Region.
   * The color should be visible and distinct from other colors so generated.
   * Currently, hues are in the interval [0, 1], saturations [0.5, 1], and 
   * brightnesses [0.5, 1]. This prevents the color from being too close to
   * black or white, but hopefully gives enough different colors that we're not
   * likely to have two regions look the same color.
   */
  public static Color generateRandomColor() {
    return Color.getHSBColor((float)Math.random(),
                             (float)Math.random() / 2.0f + 0.5f,
			     (float)Math.random() / 2.0f + 0.5f);
  }

  public boolean isHighlighted() { return highlighted; }
  public void setHighlighted(boolean h) {
    if (h != highlighted) {
      highlighted = h;
      page.emit(new Page.Event((h ? Page.Event.Type.REGION_HIGHLIGHTED : Page.Event.Type.REGION_UNHIGHLIGHTED), this));
    }
  }

  /** Return true iff this region contains the pixel at coordinates x,y.
   */
  public boolean contains(double x, double y) {
    return rect.contains(x, y);
  }

  /** Paint a rectangle representing this region.
   */
  public void paint(Graphics g) {
    if (highlighted) {
      Color oldColor = g.getColor(); // not sure whether I need to do this

      // first, fill the rectangle with a half-transparent version of the color
      g.setColor(new Color(color.getRed(), color.getGreen(), color.getBlue(),
                 63));
      g.fillRect((int)getMinX() + 1, (int)getMinY() + 1,
		 Math.max(0,(int)getAbsWidth() - 2),
		 Math.max(0,(int)getAbsHeight() - 2));
      // then, outline the rectangle with the non-transparent version
      g.setColor(color);
      g.drawRect((int)getMinX(), (int)getMinY(),
		 Math.max(0,(int)getAbsWidth() - 1),
		 Math.max(0,(int)getAbsHeight() - 1));

      g.setColor(oldColor);
    }
  }

  /** Return an equivalent Tabula Page object. Note that Tabula "Page"s can be
   * either the whole page or an area of the page. This gets the latter.
   */
  public technology.tabula.Page toTabulaPage() {
    return page.toTabulaPage().getArea(rect);
  }

  /** Return a KQML representation of this region.
   */
  @Override public KQMLObject toKQML() {
    KQMLList reg = new KQMLList();
    reg.add("rectangle");
    reg.add(":id"); reg.add(getID());
    reg.add(":page"); reg.add(page.toKQML());
    reg.add(":x"); reg.add(Double.toString(getMinX()));
    reg.add(":y"); reg.add(Double.toString(getMinY()));
    reg.add(":w"); reg.add(Double.toString(getAbsWidth()));
    reg.add(":h"); reg.add(Double.toString(getAbsHeight()));
    return reg;
  }

  /** Construct a region from its KQML representation, or fetch it from its ID.
   */
  public static Region fromKQML(KQMLObject listOrID) throws CWCException, KQMLBadPerformativeException {
    if (listOrID instanceof KQMLList) {
      return fromKQML(new KQMLPerformative((KQMLList)listOrID));
    } else if (listOrID instanceof KQMLPerformative) {
      KQMLPerformative perf = (KQMLPerformative)listOrID;
      KQMLToken idKQML =
	Args.getTypedArgument(perf, ":id", KQMLToken.class,
			      new KQMLToken("nil"));
      String id = idKQML.toString().toLowerCase();
      if (!id.equals("nil")) {
	// this is kind of a shady thing to do
	return fromKQML(idKQML);
      } else {
	double x1 = Args.getTypedArgument(perf, ":x", Double.class);
	double y1 = Args.getTypedArgument(perf, ":y", Double.class);
	double x2 = x1 + Args.getTypedArgument(perf, ":w", Double.class);
	double y2 = y1 + Args.getTypedArgument(perf, ":h", Double.class);
	KQMLObject pageKQML =
	  Args.getTypedArgument(perf, ":page", KQMLObject.class);
	Page page = Page.fromKQML(pageKQML);
        return new Region(x1,y1, x2,y2, page);
      }
    } else if (listOrID instanceof KQMLToken) {
      return HasID.get(listOrID.toString(), Region.class);
    } else {
      throw new InvalidArgument("nil", ":region", "list or id", listOrID);
    }
  }
}
