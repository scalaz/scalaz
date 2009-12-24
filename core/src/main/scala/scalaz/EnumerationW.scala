package scalaz

import java.util.Enumeration

sealed trait EnumerationW[A] extends PimpedType[Enumeration[A]] {
  def elements = new Iterator[A] {
    def hasNext = value.hasMoreElements
    def next = value.nextElement
  }
}

trait Enumerations {
  implicit def EnumerationTo[A](v: Enumeration[A]): EnumerationW[A] = new EnumerationW[A] {
    val value = v
  }
}
