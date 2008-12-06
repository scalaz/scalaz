// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.javas

/**
 * Functions over <code>java.util.List</code>.
 *
 * @see java.util.List
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object List {
  /**
   * Converts a <code>java.util.List</code> to a scala list.
   */
  implicit def JavaList_ScalaList[A](xs: java.util.List[A]): scala.List[A] = xs match {
    case NonEmpty(h, t) => h :: t
    case Empty(_) => Nil
  } 

  /**
   * Converts a scala list to a <code>java.util.List</code>.
   */
  implicit def ScalaList_JavaList[A](xs: scala.List[A]): java.util.List[A] = {
    val l = new java.util.LinkedList[A]
    xs.foreach(x => l.add(x))
    l
  }

  /**
   * Provides a non-empty extractor for <code>java.util.List</code>.
   *
   * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
   * @version $LastChangedRevision$<br>
   *          $LastChangedDate$<br>
   *          $LastChangedBy$
   */
  object NonEmpty {
    /**
     * An extractor that returns a pair of the first element of a list and the remainder of the list if the list is not
     * empty, otherwise, no match.
     */
    def unapply[A](xs: java.util.List[A]): Option[(A, java.util.List[A])] =
      if(xs.isEmpty) None
      else Some((xs.get(0), xs.subList(1, xs.size) ))
  }
  
  /**
   * Provides an empty extractor for <code>java.util.List</code>.
   *
   * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
   * @version $LastChangedRevision$<br>
   *          $LastChangedDate$<br>
   *          $LastChangedBy$
   */
  object Empty {
    /**
     * An extractor that returns <code>Some</code> if the given list is empty, <code>None</code> (no match) otherwise.
     */  
    def unapply[A](xs: java.util.List[A]): Option[Unit] = 
      if(xs.isEmpty) Some(())
      else None
  }    
}
