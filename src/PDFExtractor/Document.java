package TRIPS.PDFExtractor;

import java.util.ArrayList;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.pdmodel.PDPageTree;
import TRIPS.KQML.KQMLBadPerformativeException;
import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;
import TRIPS.KQML.KQMLPerformative;
import TRIPS.KQML.KQMLToken;
import TRIPS.util.cwc.Args;
import TRIPS.util.cwc.CWCException;
import TRIPS.util.cwc.InvalidArgument;

public class Document implements HasID {
  final String id;
  @Override public String getID() { return id; }
  final PDDocument pdDoc;
  ArrayList<Page> pages;

  public Document(PDDocument pdDoc) {
    id = HasID.getNextIDAndPut(this);
    this.pdDoc = pdDoc;
    PDPageTree pdPages = pdDoc.getPages();
    pages = new ArrayList<Page>(pdPages.getCount());
    int pageIndex = 0;
    for (PDPage pdPage : pdPages) {
      pages.add(new Page(pdPage, this, pageIndex));
      pageIndex++;
    }
  }

  public PDDocument getPDDocument() { return pdDoc; }

  public Page getPage(int pageIndex) { return pages.get(pageIndex); }

  @Override public KQMLObject toKQML() {
    KQMLPerformative p = new KQMLPerformative("document");
    p.setParameter(":id", id);
    // TODO filename?
    return p;
  }

  public static Document fromKQML(KQMLObject listOrID) throws CWCException, KQMLBadPerformativeException {
    if (listOrID instanceof KQMLList) {
      return fromKQML(new KQMLPerformative((KQMLList)listOrID));
    } else if (listOrID instanceof KQMLList) {
      KQMLPerformative perf = (KQMLPerformative)listOrID;
      KQMLToken idKQML = Args.getTypedArgument(perf, ":id", KQMLToken.class);
      // NOTE: no way to make a new document from args, just have to look it up
      return HasID.get(idKQML.toString(), Document.class);
    } else if (listOrID instanceof KQMLToken) {
      return HasID.get(listOrID.toString(), Document.class);
    } else {
      throw new InvalidArgument("nil", ":document", "a list or id", listOrID);
    }
  }
}
