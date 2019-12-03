package TRIPS.DocumentRepo;

import java.io.BufferedReader;
import java.io.Closeable;
import java.io.EOFException;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/** Poor man's JDBC for SQLite3. Runs SQL queries via the sqlite3 command.
 * I didn't use JDBC here because I didn't want to have to deal with installing
 * one of the two different SQLite3 drivers, or their licenses, and we don't
 * really need this to be fast or handle large amounts of data incrementally.
 *
 * USAGE:
 * DB db = new DB("/path/to/database.db");
 * List&lt;String[]&gt; rows =
 *   db.execute("SELECT foo, bar FROM tab WHERE baz=? AND glarch=?;",
 *		baz, glarch);
 * for (String[] row : rows) {
 *   int foo = DB.fromSQL(Integer.class, row[0]);
 *   String bar = DB.fromSQL(String.class, row[1]);
 *   ...
 * }
 */
public class DB implements Closeable {
  Process sqlite;
  PrintWriter toSQLite;
  BufferedReader fromSQLite;
  List<Listener> listeners;

  public DB(String filename) throws IOException {
    ProcessBuilder pb = new ProcessBuilder("sqlite3", filename);
    sqlite = pb.start();
    // wrap stdio streams
    toSQLite = new PrintWriter(sqlite.getOutputStream(), true);
    fromSQLite = new BufferedReader(new InputStreamReader(sqlite.getInputStream()));
    // forward sqlite's stderr to System.err
    new Thread(new StreamForward(sqlite.getErrorStream(), System.err)).start();
    listeners = new LinkedList<Listener>();
  }

  @Override public void close() throws IOException { toSQLite.close(); }

  public List<String[]> execute(String format, Object... args) throws IOException {
    String stmt = formatStatement(format, args);
//    System.err.println("sending statement " + stmt);
    toSQLite.println(stmt);
    toSQLite.println(".print"); // make SQLite print a blank line to indicate the end of the result table
    List<String[]> rows = readRows();
    if (!stmt.startsWith("SELECT ")) // FIXME case/whitespace differences?
      fireChanged();
    return rows;
  }

  /** Format an SQL statement, replacing each ? in the format with one of the
   * args.
   */
  public static String formatStatement(String format, Object[] args) {
    String stmt = format;
    for (Object arg : args)
      stmt = stmt.replaceFirst("\\?", toSQL(arg)); // FIXME what about quoted ?s
    return stmt;
  }

  public List<String[]> readRows() throws IOException {
    List<String[]> rows = new LinkedList<String[]>();
    while (true) {
      // look at the next char to see if it's the end marker
      char[] buf = new char[1];
      fromSQLite.mark(buf.length);
      int readLen = fromSQLite.read(buf, 0, buf.length);
//      System.err.println("read " + readLen + " chars: " + new String(buf));
      if (readLen == -1) {
	throw new EOFException();
      } else if (readLen == buf.length && "\n".equals(new String(buf))) {
	break;
      }
      // it's not the end marker; go back and read a full line
      fromSQLite.reset();
//      System.err.println("reading a row...");
      String line = fromSQLite.readLine();
//      System.err.println("read row: " + line);
      String[] row = line.split("\\|", -1); // FIXME escaping? maybe use .mode html or csv so that sqlite will escape its output?
//      System.err.println("row has " + row.length + " columns");
      rows.add(row);
    }
    return rows;
  }

  // FIXME handling NULL like this won't work because "col=NULL" doesn't do what you think; need to do "col IS NULL"

  /** Convert a Java String to an SQL expression for that string. */
  public static String toSQL(String val) {
    if (val == null) return "NULL";
    return new StringBuilder().
      append("'").append(val.replaceAll("'", "''")).append("'").
    toString();
  }

  /** Convert a Java Number to an SQL expression for that number. */
  public static String toSQL(Number val) {
    if (val == null) return "NULL";
    return val.toString();
  }

  /** Convert a Java List to an SQL expression for that list. */
  public static String toSQL(List val) {
    StringBuilder sb = new StringBuilder();
    sb.append("(");
    boolean first = true;
    for (Object v : val) {
      if (first) {
	first = false;
      } else {
	sb.append(", ");
      }
      sb.append(toSQL(v));
    }
    sb.append(")");
    return sb.toString();
  }

  /** Convert a Java Map&lt;String,String&gt; to a sequence of assignments for
   * an SQL UPDATE statement.
   */
  public static String toSQL(Map<String,String> val) {
    StringBuilder sb = new StringBuilder();
    boolean first = true;
    for (String k : val.keySet()) {
      if (first) {
	first = false;
      } else {
	sb.append(", ");
      }
      sb.append(toSQL(k)).append("=").append(toSQL(val.get(k)));
    }
    return sb.toString();
  }

  // java, y u no dispatch on runtime argument types?
  public static String toSQL(Object val) {
    if (val instanceof String) {
      return toSQL((String)val);
    } else if (val instanceof Number) {
      return toSQL((Number)val);
    } else if (val instanceof List) {
      return toSQL((List)val);
    } else if (val instanceof Map) {
      return toSQL((Map<String,String>)val); // ick.
    } else {
      throw new IllegalArgumentException("expected a String, a Number, or a List or Map thereof, but got a " + val.getClass().getName());
    }
  }

  /** Convert a String from a table printed by sqlite to a value of the given
   * Java type (String, Integer, or Double).
   */
  public static <T> T fromSQL(Class<T> t, String str) {
    if (t.equals(String.class)) {
      if (str.isEmpty()) { // FIXME '' IS NULL?
	return t.cast(null);
      } else {
	return t.cast(str); // FIXME unescape? (by default sqlite doesn't escape output)
      }
    } else if (t.equals(Integer.class)) {
      return t.cast(Integer.valueOf(str));
    } else if (t.equals(Double.class)) {
      return t.cast(Double.valueOf(str));
    } else {
      throw new IllegalArgumentException("fromSQL can convert to String, Integer, or Double, not to " + t.getName());
    }
  }

  public void addListener(Listener l) {
    listeners.add(l);
  }

  public void fireChanged() {
    for (Listener l : listeners) l.changed();
  }

  public interface Listener {
    void changed();
  }
}
