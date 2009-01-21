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
  def |*|[X](e: => X) = new {
    def |**|(f: NonEmptyList[A] => X) = list match {
      case Nil => e
      case h :: t => f(NonEmptyList.nel(h, t))
    }
  }

  import xml.{Node, Text}

  /**
   * Returns an empty XML text node if this is an empty list or runs the given function on the head and tail.
   */
  def  |-|(f: NonEmptyList[A] => Node) = |*|(Text(""), f)
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
