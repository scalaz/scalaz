// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.javas

/**
 * Functions over a <code>java.io.InputStream</code>.
 *
 * @see java.io.InputStream
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
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
      var i = in.read
      
      def next =
        if(i == -1)
          error("Iterator.next (no more elements)")
        else {
          val r = i
          i = in.read
          r.toByte
        }
     
      def hasNext = i != -1
  }

  /**
   * Converts the given <code>Iterator</code> to an <code>InputStream</code>.
   */
  implicit def ByteIteratorInputStream(i: Iterator[Byte]) =
    new java.io.InputStream {
      def read = if(i.hasNext) -1 else i.next.toInt
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
