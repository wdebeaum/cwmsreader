package TRIPS.PDFExtractor;

import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.List;
import technology.tabula.HasText;
import technology.tabula.RectangularTextContainer;

/** Replacement for the TextChunk cells we get from Tabula and the .merge()
 * method, which properly merges the text contents of the subcells, joining
 * them with spaces and newlines.
 */
public class MergedCell extends RectangularTextContainer<HasText> implements HasText {
  List<HasText> textElements;
  boolean horizontally;
  public MergedCell(List<HasText> textElements, boolean horizontally) {
    // Ugghhhh, java y u no let super come later?
    super(aggregateTop(textElements), aggregateLeft(),
          aggregateWidth(), aggregateHeight());
    this.textElements = textElements;
    this.horizontally = horizontally;
  }

  public static MergedCell fromRTCs(List<RectangularTextContainer> textElements, boolean horizontally) {
    List<HasText> copy = new ArrayList<HasText>(textElements.size());
    for (RectangularTextContainer rtc : textElements) {
      copy.add((HasText)rtc);
    }
    return new MergedCell(copy, horizontally);
  }

  @Override public String getText() {
    List<String> texts = new ArrayList<String>(textElements.size());
    for (HasText element : textElements) {
      texts.add(element.getText());
    }
    if (horizontally) {
      List<List<String>> rows = new ArrayList<List<String>>();
      for (String text : texts) {
	String[] lines = text.split("\n");
	int i = 0;
	for (String line : lines) {
	  if (rows.size() <= i)
	    rows.add(new ArrayList<String>());
	  rows.get(i).add(line);
	  i++;
	}
      }
      List<String> mergedLines = new ArrayList<String>(rows.size());
      for (List<String> row : rows) {
	mergedLines.add(String.join(" ", row));
      }
      return String.join("\n", mergedLines);
    } else {
      return String.join("\n", texts);
    }
  }
  
  // Tabula doesn't do this right, why should we?
  @Override public String getText(boolean useLineReturns) { return null; }

  @Override public List<HasText> getTextElements() {
    return textElements;
  }

  // "Ugghhhh" cont'd
  private static Rectangle2D.Float aggregate;
  private static float aggregateTop(List<HasText> elems) {
    aggregate = new Rectangle2D.Float();
    aggregate.setRect((RectangularTextContainer)elems.get(0));
    for (HasText e : elems.subList(1, elems.size())) {
      aggregate.setRect(aggregate.createUnion((RectangularTextContainer)e));
    }
    return (float)aggregate.getY();
  }
  private static float aggregateLeft() { return (float)aggregate.getX(); }
  private static float aggregateWidth() { return (float)aggregate.getWidth(); }
  private static float aggregateHeight() { return (float)aggregate.getHeight();}
}
