package scalaz

import java.util.Enumeration

sealed trait EnumerationW[A] {
  val value: Enumeration[A]

  def elements = new Iterator[A] {
    def hasNext = value.hasMoreElements
    def next = value.nextElement
  }
}

trait Enumerations {
  implicit def EnumerationTo[A](v: Enumeration[A]): EnumerationW[A] = new EnumerationW[A] {
    val value = v
  }

  implicit def EnumerationFrom[A](v: EnumerationW[A]): Enumeration[A] = v.value
}
