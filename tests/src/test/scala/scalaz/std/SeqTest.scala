package scalaz
package std

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import Id._
import syntax.std._

class SeqTest extends Spec {
  checkAll(equal.laws[Seq[Int]])
  checkAll(monoid.laws[Seq[Int]])
  checkAll(monadPlus.strongLaws[Seq])
  checkAll(traverse.laws[Seq])
  checkAll(isEmpty.laws[Seq])
  checkAll(order.laws[Seq[Int]])

  import std.seq.seqSyntax._
  import syntax.foldable._
  import syntax.monad._

  "intercalate empty seq is flatten" ! check((a: Seq[Seq[Int]]) ⇒
    a.intercalate(Seq[Int]()) must be_===(a.flatten))

  "intersperse then remove odd items is identity" ! prop {
    (a: Seq[Int], b: Int) ⇒
      val isEven = (_: Int) % 2 == 0
      a.intersperse(b).zipWithIndex.filter(p ⇒ isEven(p._2)).map(_._1) must be_===(a)
  }

  "intercalate is same as a.intersperse(b).flatten" ! check  {
    (a: Seq[Seq[Int]], b: Seq[Int]) ⇒
      a.intercalate(b) must be_===(a.intersperse(b).flatten)
  }

  "intersperse vs benchmark" ! check  {
    def intersperse[A](value: Seq[A], a: A): Seq[A] = value match {
      case Seq()        ⇒ Seq()
      case Seq(x)       ⇒ Seq(x)
      case Seq(h, t@_*) ⇒ h +: a +: intersperse(t, a)
    }
    (a: Seq[Int], b: Int) ⇒ (a.intersperse(b) must be_===(intersperse(a, b)))
  }

  "groupByM[Id].flatten is identity" ! prop {
    (a: Seq[Int], p: (Int, Int) ⇒ Boolean) ⇒
      a.groupByM[Id](p).flatten must be_===(a)
  }

  "groupByWhen.flatten is identity" ! prop {
    (a: Seq[Int], p: (Int, Int) ⇒ Boolean) ⇒
      a.groupWhen(p).flatten must be_===(a)
  }

  "filterM" ! prop {
    (xs: Seq[Int]) ⇒ xs.filterM[Id](_ % 2 == 0) == xs.filter(_ % 2 == 0)
  }

  "takeWhileM example" in {
    def takeWhileN[A](as: Seq[A], n: Int)(f: A ⇒ Boolean): Seq[A] =
      as.takeWhileM[({type λ[α] = State[Int, α]})#λ](a ⇒ State {
        i ⇒
          val j = i + (if (f(a)) 0 else 1)
          val done = j >= n
          (j, !done)
      }).evalZero[Int]

    val actual = takeWhileN("/abc/def/hij/klm".toSeq, 4)(_ != '/').mkString
    actual must be_===("/abc/def/hij")
  }

  "foldl is foldLeft" ! prop {(rnge: Seq[Seq[Int]]) ⇒
    val F = Foldable[Seq]
    (rnge.foldLeft(Seq[Int]())(_ ++ _)
      must be_===(F.foldLeft(rnge, Seq[Int]())(_ ++ _)))
  }

  "foldr is foldRight" ! prop {(rnge: Seq[Seq[Int]]) ⇒
    val F = Foldable[Seq]
    (rnge.foldRight(Seq[Int]())(_ ++ _)
      must be_===(F.foldRight(rnge, Seq[Int]())(_ ++ _)))
  }
}
