package TRIPS.DocumentRepo;

import java.awt.BorderLayout;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.*;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.TransferHandler;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableModel;
import TRIPS.KQML.*;

/** Display a list of documents for the user to browse and select from. */
public class DocumentList extends JPanel implements ListSelectionListener, DB.Listener {
  final String[] tableFields = {"file", "title", "created"};
  final String tableSelect =
    "SELECT " + String.join(",", tableFields) + " FROM documents;";
  DocumentRepo repo;
  DefaultTableModel model;
  ListSelectionModel selection;
  JTable view;
  JLabel details;
  DLTransferHandler transferHandler;

  public DocumentList(DocumentRepo repo) throws IOException {
    super(new BorderLayout());
    this.repo = repo;
    model = new DefaultTableModel();
    // TODO only show a few fields in the table; save the rest for details
    for (String field : tableFields) {
      model.addColumn(field);
    }
    updateModel();
    repo.db.addListener(this);
    view = new JTable(model);
    transferHandler = new DLTransferHandler();
    view.setTransferHandler(transferHandler);
    view.setDragEnabled(true);
    selection = view.getSelectionModel();
    selection.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    selection.addListSelectionListener(this);
    JScrollPane scrollPane = new JScrollPane(view);
    add(scrollPane);
    details = new JLabel();
    updateDetails();
    details.setTransferHandler(transferHandler);
    details.addMouseListener(new DetailsMouseListener());
    add(details, BorderLayout.SOUTH);
  }

  void updateModel() throws IOException {
    model.setRowCount(0); // clear the table
    for (String[] row : repo.db.execute(tableSelect)) {
      model.addRow(row);
    }
  }

  void updateDetails() throws IOException {
    // get the full row of data for the selected file, if any
    int i = selection.getMinSelectionIndex();
    String[] row;
    if (i == -1) { // empty selection
      row = new String[repo.fields.size()];
      Arrays.fill(row, "");
    } else { // row selected
      String filename = (String)model.getValueAt(i, 0);
      List<String[]> rows =
        repo.db.execute("SELECT * FROM documents WHERE file=?;", filename);
      if (rows.size() != 1)
	throw new RuntimeException("expected exactly one row in documents table for file " + filename + ", but got " + rows.size());
      row = rows.get(0);
      if (row.length != repo.fields.size())
	throw new RuntimeException("expected a row of length " + repo.fields.size() + " in documents, but got a row of length " + row.length);
    }
    // build an html table with a row for each field
    StringBuilder b = new StringBuilder();
    b.append("<html><table>");
    int j = 0;
    for (String field : repo.fields) {
      b.append("<tr><th align=\"right\">").append(field).append(": <td>").append(row[j]);
      j++;
    }
    b.append("</table></html>");
    details.setText(b.toString());
  }

  // ListSelectionListener
  @Override public void valueChanged(ListSelectionEvent evt) {
    if (evt.getValueIsAdjusting()) return;
    try {
      updateDetails();
    } catch (IOException ex) { ex.printStackTrace(); }
    int i = selection.getMinSelectionIndex();
    if (i != -1) { // nonempty selection
      KQMLPerformative selected = new KQMLPerformative("selected");
      KQMLPerformative file = new KQMLPerformative("file");
      file.setParameter(":name", new KQMLString((String)model.getValueAt(i,0)));
      file.setParameter(":format", (String)model.getValueAt(i,1));
      selected.setParameter(":what", file);
      selected.setParameter(":who", "usr");
      repo.report(selected);
    }
  }

  // DB.Listener
  @Override public void changed() {
    SwingUtilities.invokeLater(new Runnable() {
      @Override public void run() {
	try {
	  updateModel();
	  updateDetails();
	} catch (IOException ex) {
	  System.err.println("failed to update table model from db");
	  ex.printStackTrace();
	}
      }
    });
  }

  class DetailsMouseListener extends MouseAdapter {
    @Override public void mousePressed(MouseEvent evt) {
      transferHandler.exportAsDrag(details, evt, TransferHandler.COPY);
    }
  }

  class DLTransferable implements Transferable {
    String filename;
    public DLTransferable() {
      int i = selection.getMinSelectionIndex();
      if (i == -1)
	throw new RuntimeException("empty selection");
      filename = (String)model.getValueAt(i,0);
    }

    @Override public Object getTransferData(DataFlavor flavor) {
      ArrayList<File> ret = new ArrayList<File>(1);
      ret.add(new File(filename));
      return ret;
    }

    @Override public DataFlavor[] getTransferDataFlavors() {
      DataFlavor[] ret = {DataFlavor.javaFileListFlavor};
      return ret;
    }

    @Override public boolean isDataFlavorSupported(DataFlavor flavor) {
      return flavor.isFlavorJavaFileListType();
    }
  }

  // support dragging files onto this panel as a way of adding them to the repo
  class DLTransferHandler extends TransferHandler {
    @Override public boolean canImport(JComponent comp, DataFlavor[] flavors) {
      for (DataFlavor f : flavors) {
	if (f.isFlavorJavaFileListType())
	  return true;
      }
      return false;
    }

    @Override public boolean importData(JComponent comp, Transferable t) {
      try {
	List<File> files =
	  (List<File>)t.getTransferData(DataFlavor.javaFileListFlavor);
	for (File f : files) {
	  System.err.println("got dropped file " + f.toString());
	  KQMLObject format = DocumentRepo.guessFileFormat(f);
	  System.err.println("  guessed format " + format.toString());
	  repo.db.execute("INSERT INTO documents (file, format) VALUES (?, ?);",
	                  f.toString(), format.toString());
	  System.err.println("inserted into DB");
	}
	return true;
      } catch (Exception ex) {
	System.err.println("WTF");
	ex.printStackTrace();
	return false;
      }
    }

    @Override public int getSourceActions(JComponent c) {
      return TransferHandler.COPY;
    }

    @Override protected Transferable createTransferable(JComponent c) {
      return new DLTransferable();
    }
  }

  // TODO
  // - correct column widths
}
