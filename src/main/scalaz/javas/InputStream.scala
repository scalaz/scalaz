// Copyright Tony Morris 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.javas

/**
 * Functions over a <code>java.io.InputStream</code>.
 *
 * @see java.io.InputStream
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object InputStream {
  /**
   * Converts the given <code>InputStream</code> to an iterator.
   */
  implicit def InputStreamByteIterator(in: java.io.InputStream) = 
    new Iterator[Byte] {
      var i: Int = _
      var b = false
      var h = true
      
      def next = if(hasNext) {
        b = false
        i.toByte
      } else error("Iterator.next (no more elements)")
     
      def hasNext = {
        if(b) h
        else if(h) {
          i = in.read
          b = true
          if(i == -1)
            h = false
          h
        } else false
      }
  }

  /**
   * Converts the given <code>Iterator</code> to an <code>InputStream</code>.
   */
  implicit def ByteIteratorInputStream(i: Iterator[Byte]) =
    new java.io.InputStream {
      def read = if(i.hasNext) i.next.toInt else -1
    }

  /**
   * Converts the given <code>InputStream</code> to a stream.
   */
  implicit def InputStreamByteStream(in: java.io.InputStream): Stream[Byte] = {
      val c = in.read
      if(c == -1) Stream.empty
      else Stream.cons(c.toByte, in)
    }
}
