// Copyright Tony Morris 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.javas

/**
 * Functions over a <code>java.io.Reader</code>.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Reader {
  /**
   * Converts the given <code>Reader</code> to an iterator.
   */
  implicit def ReaderCharIterator(r: java.io.Reader) = new Iterator[Char] {
    var i = r.read

    def next =
      if(i == -1)
        error("Iterator.next (no more elements)")
      else {
        val x = i
        i = r.read
        x.toChar
      }

    def hasNext = i != -1    
  }
}
