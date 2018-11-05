package TRIPS.PDFExtractor;

import java.io.File;
import java.io.IOException;
import java.util.EnumSet;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import javax.swing.JComponent;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.common.PDRectangle;
import org.apache.pdfbox.rendering.PDFRenderer;

public class PDFPane extends JComponent implements Page.Listener {
  Document doc;
  PDFRenderer renderer;
  final int numPages;
  int pageIndex;
  public PDFPane(File pdfFile, int pageIndex) throws IOException {
    setFocusable(true);
    setOpaque(true);
    setCursor(Cursor.getPredefinedCursor(Cursor.CROSSHAIR_CURSOR));
    PDDocument pdDoc = PDDocument.load(pdfFile);
    doc = new Document(pdDoc);
    renderer = new PDFRenderer(pdDoc);
    numPages = pdDoc.getPages().getCount();
    setPage(pageIndex);
  }

  public int getNumPages() { return numPages; }

  public void setPage(int pageIndex) {
    if (pageIndex < 0 || pageIndex >= numPages) {
      throw new ArrayIndexOutOfBoundsException(pageIndex);
    }
    this.pageIndex = pageIndex;
    Page page = doc.getPage(pageIndex);
    // setPreferredSize according to page dims
    PDRectangle bbox = page.getPDBBox();
    setPreferredSize(new Dimension((int)bbox.getWidth(), (int)bbox.getHeight()));
    page.addPageListener(this);
    repaint();
  }

  public void incPage(int pageInc) {
    // try to increment by pageInc
    try {
      setPage(this.pageIndex + pageInc);
    } catch (ArrayIndexOutOfBoundsException e) {
      // fall back to first or last page, depending on direction
      if (pageInc < 0) {
	setPage(0);
      } else {
	setPage(numPages - 1);
      }
    }
  }

  public Page getPage() {
    return doc.getPage(pageIndex);
  }

  /** Get the resize cursor to display when the mouse is at (x,y), or a
   * crosshair cursor when no resize handle is at (x,y). The result will agree
   * with getPage().getRegionAt(x,y).
   */
  public Cursor getCursorAt(int x, int y) {
    Region region = getPage().getRegionAt(x, y);
    if (region == null)
      return Cursor.getPredefinedCursor(Cursor.CROSSHAIR_CURSOR);
    EnumSet<Region.Coord> handle = region.getHandleAt((double)x, (double)y);
    if (handle.contains(Region.Coord.X1)) {
      if (handle.contains(Region.Coord.Y1)) {
	return Cursor.getPredefinedCursor(Cursor.NW_RESIZE_CURSOR);
      } else if (handle.contains(Region.Coord.Y2)) {
	return Cursor.getPredefinedCursor(Cursor.SW_RESIZE_CURSOR);
      } else {
	return Cursor.getPredefinedCursor(Cursor.W_RESIZE_CURSOR);
      }
    } else if (handle.contains(Region.Coord.X2)) {
      if (handle.contains(Region.Coord.Y1)) {
	return Cursor.getPredefinedCursor(Cursor.NE_RESIZE_CURSOR);
      } else if (handle.contains(Region.Coord.Y2)) {
	return Cursor.getPredefinedCursor(Cursor.SE_RESIZE_CURSOR);
      } else {
	return Cursor.getPredefinedCursor(Cursor.E_RESIZE_CURSOR);
      }
    } else {
      if (handle.contains(Region.Coord.Y1)) {
	return Cursor.getPredefinedCursor(Cursor.N_RESIZE_CURSOR);
      } else if (handle.contains(Region.Coord.Y2)) {
	return Cursor.getPredefinedCursor(Cursor.S_RESIZE_CURSOR);
      } else {
	return Cursor.getPredefinedCursor(Cursor.CROSSHAIR_CURSOR);
      }
    }
  }

  @Override
  public void paintComponent(Graphics g) {
    try {
      // make a copy of g so that renderPageToGraphics doesn't mess up the
      // transform for us
      Graphics2D g2d = (Graphics2D)(g.create());
      renderer.renderPageToGraphics(pageIndex, g2d);
      for (Region region : getPage().getRegions()) {
	region.paint(g);
      }
    } catch (IOException e) {
      System.err.println(e);
    }
  }

  @Override
  public void pageChanged(Page.Event evt) {
    if (evt.getPage().pageIndex == pageIndex) {
      repaint();
    }
  }
}
