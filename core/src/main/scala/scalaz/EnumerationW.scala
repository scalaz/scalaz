package scalaz

import java.util.Enumeration

sealed trait EnumerationW[A] {
  val value: Enumeration[A]

  def elements = new Iterator[A] {
    def hasNext = value.hasMoreElements
    def next = value.nextElement    
  }
}

object EnumerationW {
  implicit def EnumerationTo[A](v: Enumeration[A]) = new EnumerationW[A] {
    val value = v
  }

  implicit def EnumerationFrom[A](v: EnumerationW[A]) = v.value  
}
