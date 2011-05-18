package scalaz
package wrap

import java.util.Enumeration

sealed trait EnumerationW[A] {
  val value: Enumeration[A]

  def elements = new Iterator[A] {
    def hasNext = value.hasMoreElements

    def next = value.nextElement
  }

  def stream: Stream[A] =
    elements.toStream

  def list: List[A] =
    elements.toList
}

object EnumerationW extends EnumerationWs

trait EnumerationWs {
  implicit def EnumerationTo[A](v: Enumeration[A]): EnumerationW[A] = new EnumerationW[A] {
    val value = v
  }
}
