package TRIPS.DocumentRepo;

import java.io.File;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.regex.*;
import javax.swing.JFrame;
import TRIPS.KQML.*;
import TRIPS.util.cwc.Args;
import TRIPS.util.cwc.CWCException;
import TRIPS.util.cwc.InvalidArgument;
import TRIPS.util.cwc.StandardCWCModule;
import TRIPS.util.cwc.SwingWindowManager;
import TRIPS.util.cwc.UnknownObject;
import TRIPS.util.cwc.WindowConfig;

public class DocumentRepo extends StandardCWCModule {
  DB db;
  List<String> fields;
  List<String> fieldGlosses;

  public DocumentRepo(String[] argv) {
    super(argv);
  }

  @Override
  public void init() {
    name = "DocumentRepo";
    windowManager = new SwingWindowManager(this);
    try {
      db = new DB(System.getenv("TRIPS_BASE") + "/etc/" + name + "/repo.db");
      fields = new LinkedList<String>();
      fieldGlosses = new LinkedList<String>();
      Pattern colpat = Pattern.compile("^  (\\w+).*?-- (.*)$");
      for (String[] row : db.execute(".schema documents")) {
	Matcher m = colpat.matcher(row[0]);
	if (m.matches()) {
	  fields.add(m.group(1));
	  fieldGlosses.add(m.group(2));
	}
      }
      super.init();
      ready();
    } catch (IOException ex) {
      throw new RuntimeException("failed to open database", ex);
    }
  }

  @Override
  public void subscribe() throws IOException {
    send(KQMLPerformative.fromString(
      "(subscribe :content (request &key :content (put-metadata . *)))"));
    send(KQMLPerformative.fromString(
      "(subscribe :content (request &key :content (get-metadata . *)))"));
    send(KQMLPerformative.fromString(
      "(subscribe :content (request &key :content (display-document-list . *)))"));
    // TODO query, relationships
  }

  void declarePutMetadata() throws IOException {
    KQMLPerformative tell = new KQMLPerformative("tell");
    KQMLPerformative ds = new KQMLPerformative("define-service");
    ds.setParameter(":name", "put-metadata");
    ds.setParameter(":component", name);
    KQMLList inputs = new KQMLList();
    for (int i = 0; i < fields.size(); i++) {
      String field = fields.get(i);
      String gloss = fieldGlosses.get(i);
      KQMLPerformative input = new KQMLPerformative("input");
      input.setParameter(":name", field);
      input.setParameter(":gloss", new KQMLString(gloss));
      input.setParameter(":id-code", field);
      if (field.equals("file")) {
	input.setParameter(":format", "ont::file");
	input.setParameter(":requirements", ":required");
      } else {
	// TODO more detailed formats
	input.setParameter(":format", "ont::string");
	input.setParameter(":requirements", ":optional");
      }
      inputs.add(input);
    }
    ds.setParameter(":input", inputs);
    ds.setParameter(":output", "nil");
    tell.setParameter(":content", ds);
    send(tell);
  }

  void declareGetMetadata() throws IOException {
    KQMLPerformative tell = new KQMLPerformative("tell");
    KQMLPerformative ds = new KQMLPerformative("define-service");
    ds.setParameter(":name", "get-metadata");
    ds.setParameter(":component", name);
    KQMLList inputs = new KQMLList();
    KQMLPerformative input = new KQMLPerformative("input");
    input.setParameter(":name", "file");
    input.setParameter(":gloss", new KQMLString("path to file on disk"));
    input.setParameter(":id-code", "file");
    input.setParameter(":format", "ont::file");
    input.setParameter(":requirements", ":required");
    inputs.add(input);
    // TODO? add input for selecting specific fields
    ds.setParameter(":input", inputs);
    KQMLList outputs = new KQMLList();
    for (int i = 0; i < fields.size(); i++) {
      String field = fields.get(i);
      String gloss = fieldGlosses.get(i);
      KQMLPerformative output = new KQMLPerformative("output");
      output.setParameter(":name", field);
      output.setParameter(":gloss", new KQMLString(gloss));
      output.setParameter(":id-code", field);
      if (field.equals("file")) {
	output.setParameter(":format", "ont::file");
      } else {
	// TODO more detailed formats
	output.setParameter(":format", "ont::string");
      }
      outputs.add(output);
    }
    ds.setParameter(":output", outputs);
    tell.setParameter(":content", ds);
    send(tell);
  }

  @Override
  public void declareCapabilities() throws IOException {
    declarePutMetadata();
    declareGetMetadata();
    send(KQMLPerformative.fromString(
      "(tell :content (define-service :name display-document-list :component " + name + " :input nil :output nil))"));
  }

  @Override
  public boolean receiveRequest(KQMLPerformative msg, String verb, KQMLPerformative content) throws CWCException, IOException {
    if (verb.equals("put-metadata")) {
      String file = getFilenameFromRequestContent(content);
      String format = getFormatFromRequestContent(content);
      if (db.execute("SELECT 'found' FROM documents WHERE file=?;", file).size() > 0) {
	// already have this file, just update the fields we got
	// find the fields and values referenced in content, put them in reqPairs (except file)
	Map<String,String> reqPairs = new TreeMap<String,String>();
	for (String field : fields) {
	  String val = null;
	  if (field.equals("file")) {
	    continue; // skip file field since that's how we're looking it up
	  } else if (field.equals("format")) {
	    val = format;
	  } else {
	    KQMLObject kqmlVal = content.getParameter(":" + field);
	    if (kqmlVal != null)
	      val = kqmlVal.stringValue();
	  }
	  if (val != null)
	    reqPairs.put(field, val);
	}
	db.execute("UPDATE documents SET ? WHERE file=?;", reqPairs, file);
      } else {
	// new file, insert
	// find the fields and values referenced in content, put them in reqFields and reqValues
	List<String> reqFields = new LinkedList<String>();
	List<String> reqValues = new LinkedList<String>();
	for (String field : fields) {
	  String val = null;
	  if (field.equals("file")) {
	    val = file;
	  } else if (field.equals("format")) {
	    val = format;
	  } else {
	    KQMLObject kqmlVal = content.getParameter(":" + field);
	    if (kqmlVal != null)
	      val = kqmlVal.stringValue();
	  }
	  if (val != null) {
	    reqFields.add(field);
	    reqValues.add(val);
	  }
	}
	db.execute("INSERT INTO documents ? VALUES ?;", reqFields, reqValues);
      }
    } else if (verb.equals("get-metadata")) {
      String file = getFilenameFromRequestContent(content);
      List<String[]> rows =
        db.execute("SELECT * FROM documents WHERE file=?;", file);
      if (rows.size() == 0)
	throw new UnknownObject(new KQMLString(file).toString());
      String[] row = rows.get(0);
      if (row.length != fields.size())
	throw new RuntimeException("expected to get a row with " + fields.size() + " fields from the documents table, but got one with " + row.length);
      // construct answer
      KQMLPerformative answer = new KQMLPerformative("answer");
      for (int i = 0; i < row.length; i++) {
	String sqlVal = row[i];
	String javaVal = DB.fromSQL(String.class, sqlVal);
	if (javaVal != null)
	  answer.setParameter(":" + fields.get(i), new KQMLString(javaVal));
      }
      report(msg, answer);
    } else if (verb.equals("display-document-list")) {
      WindowConfig wc = new WindowConfig(content);
      if (wc.title == null) wc.title = "Document Repository";
      Map.Entry<KQMLToken,JFrame> entry =
	((SwingWindowManager)windowManager).createWindow(wc);
      JFrame window = entry.getValue();
      window.add(new DocumentList(this));
      window.pack();
      window.setVisible(true);
    } else {
      return super.receiveRequest(msg, verb, content);
    }
    return true;
  }

  public String getFilenameFromRequestContent(KQMLPerformative content) throws CWCException, KQMLException {
    KQMLObject fileKQML = Args.getTypedArgument(content, ":file", KQMLObject.class);
    if (fileKQML instanceof KQMLString) {
      return fileKQML.stringValue();
    } else if (fileKQML instanceof KQMLList) {
      return Args.getTypedArgument(new KQMLPerformative((KQMLList)fileKQML),
				   ":name", String.class);
    } else {
      throw new InvalidArgument(content, ":file", "string or (file...) structure");
    }
  }

  public String getFormatFromRequestContent(KQMLPerformative content) throws CWCException, KQMLException {
    KQMLObject formatKQML = content.getParameter(":format");
    if (formatKQML == null) {
      KQMLObject fileKQML =
	Args.getTypedArgument(content, ":file", KQMLObject.class);
      if (fileKQML instanceof KQMLList)
	formatKQML = 
	  (new KQMLPerformative((KQMLList)fileKQML)).getParameter(":format");
    }
    return (formatKQML == null ? null : formatKQML.toString());
  }

  /** Guess a file :format argument based on the end of the filename. */
  static KQMLObject guessFileFormat(File f) {
    String name = f.toString().toLowerCase();
    if (name.endsWith(".pdf")) {
      return new KQMLList(new KQMLToken("vector"), new KQMLString("PDF"));
    } else if (name.endsWith(".svg")) {
      return new KQMLList(new KQMLToken("vector"), new KQMLString("SVG"));
    } else if (name.endsWith(".txt")) {
      return new KQMLString("text/plain");
    } else if (name.endsWith(".csv")) {
      return new KQMLString("text/csv");
    // TODO? GeoJSON, shp/shx, XML, dot, ...
    } else {
      Matcher m = Pattern.compile("\\.html?").matcher(name);
      if (m.find())
	return new KQMLString("text/html");
      m = Pattern.compile("\\.(png|gif|bmp|jpe?g|g?tiff?)$").matcher(name);
      if (m.find()) {
	String formatName = m.group(1).toUpperCase();
	String width = "?", height = "?";
	m = 
	  Pattern.
	  compile("-(\\d+)x(\\d+)([\\.-]map)?\\.[a-z]+$").
	  matcher(name);
	if (m.find()) {
	  width = m.group(1);
	  height = m.group(2);
	} // TODO? else try to obtain WxH from file contents
	return new KQMLList(
	  new KQMLToken("raster"),
	  new KQMLString(formatName),
	  new KQMLToken(width),
	  new KQMLToken(height)
	);
      }
      // else, fall back on the most generic MIME type
      return new KQMLString("application/octet-stream");
    }
  }

  @Override
  public void receiveEOF() {
    try { db.close(); } catch (IOException ex) { /* ignore */ }
    super.receiveEOF();
  }

  public static void main(String[] argv) {
    new DocumentRepo(argv).run();
  }
}
