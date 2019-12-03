package TRIPS.DocumentRepo;

import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;

/** Forward bytes from an InputStream to an OutputStream. Avoid delays due to
 * buffering and blocking.
 */
public class StreamForward implements Runnable {
  InputStream from;
  OutputStream to;
  public StreamForward(InputStream from, OutputStream to) {
    this.from = from;
    this.to = to;
  }
  @Override public void run() {
    try {
      byte[] buf = new byte[1024];
      while (true) {
	int readLen = from.available(); // try to read just what's available
	if (readLen == 0) readLen = 1; // ...otherwise block on the next byte
	if (readLen > buf.length) readLen = buf.length;
	readLen = from.read(buf, 0, readLen);
	if (readLen == -1) break; // EOF
	to.write(buf, 0, readLen);
      }
    } catch (IOException ex) {
      System.err.println("error forwarding stream");
      ex.printStackTrace();
    }
  }
}

