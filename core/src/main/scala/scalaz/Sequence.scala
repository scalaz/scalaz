package scalaz

import Scalaz._
import FingerTree._

object Sequence {
  case class Elem[A](value: A) extends NewType[A]
  
  implicit def measureElem[A]: Reducer[Elem[A], Int] = Reducer(a => 1)

  /*case class Sequence[A](value: FingerTree[Int, A]) extends NewType[FingerTree[Int, A]] {
    def apply(i: Int): A  = value.split(_ > i)._1.viewr.head
  }*/
}
