// Copyright Tony Morris 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz

/**
 * Wraps <code>scala.List</code> and provides additional methods.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait ListW[A] {
  /**
   * The value of this list.
   */
  val list: List[A]

  import scalaz.list.NonEmptyList

  /**
   * Returns the first argument if this is an empty list or runs the given function on the head and tail.
   */
  def |*|[X](e: => X, f: NonEmptyList[A] => X) = list match {
    case Nil => e
    case h :: t => f(NonEmptyList.nel(h, t))
  }

  import xml.{Node, Text}

  /**
   * Returns an empty text node if this list is empty otherwise runs the given function on the head and tail. 
   */
  def |#|(f: A => Node) = |*|(Text(""), x => (x map f).toList)
}

/**
 * Functions over list values.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object ListW {
  /**
   * Unwraps a <code>scala.List</code>.
   */
  implicit def ListWList[A](as: ListW[A]) = as.list

  /**
   * Wraps a <code>scala.List</code>.
   */
  implicit def ListListW[A](as: List[A]) = new ListW[A] {
    val list = as
  }
}
