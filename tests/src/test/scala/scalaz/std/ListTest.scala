package scalaz
package std

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import Id._

class ListTest extends Spec {
  checkAll(equal.laws[List[Int]])
  checkAll(monoid.laws[List[Int]])
  checkAll(monadPlus.strongLaws[List])
  checkAll(traverse.laws[List])
  
  import std.list.listSyntax._
  import syntax.monad._

  "intercalate empty list is identity" ! check((a: List[Int]) => a.intercalate(Nil) must be_===(a))

  "intersperse then remove odd items is identity" ! check {
    (a: List[Int], b: Int) =>
      val isEven = (_: Int) % 2 == 0
      a.intersperse(b).zipWithIndex.filter(p => isEven(p._2)).map(_._1) must be_===(a)
  }

  "intercalate single element lists the same as intersperse" ! check  {
    (a: List[Int], b: Int) =>
      a.intercalate(List(b)) must be_===(a.intersperse(b))
  }

  "intersperse vs benchmark" ! check  {
    def intersperse[A](value: List[A], a: A): List[A] = value match {
      case Nil      => Nil
      case x :: Nil => x :: Nil
      case h :: t   => h :: a :: intersperse(t, a)
    }
    (a: List[Int], b: Int) => (a.intersperse(b) must be_===(intersperse(a, b)))
  }

  "intercalate vs benchmark" ! check  {
    def intercalate[A](value: List[A], as: List[A]): List[A] = value match {
      case Nil      => Nil
      case x :: Nil => x :: Nil
      case h :: t   => h :: as ::: intercalate(t, as)
    }
    (a: List[Int], b: List[Int]) => (a.intercalate(b) must be_===(intercalate(a, b)))
  }

  "groupByM[Id].flatten is identity" ! check {
    (a: List[Int], p: (Int, Int) => Boolean) =>
      a.groupByM[Id]((a, b) => p(a, b)).flatten must be_===(a)
  }

  "takeWhileM example" in {
    def takeWhileN[A](as: List[A], n: Int)(f: A => Boolean): List[A] = as.takeWhileM[({type λ[α] = State[Int, α]})#λ](a => State {
      i =>
        val j = i + (if (f(a)) 0 else 1)
        val done = j >= n
        (!done, j)
    }).evalZero

    val actual = takeWhileN("/abc/def/hij/klm".toList, 4)(_ != '/').mkString
    actual must be_===("/abc/def/hij")
  }
}
