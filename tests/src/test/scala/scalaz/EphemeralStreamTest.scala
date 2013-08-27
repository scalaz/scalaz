package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._
import syntax.contravariant._

class EphemeralStreamTest extends Spec {

  checkAll(equal.laws[EphemeralStream[Int]])
  checkAll(monadPlus.laws[EphemeralStream])
  checkAll(traverse.laws[EphemeralStream])

  implicit def ephemeralStreamShow[A: Show]: Show[EphemeralStream[A]] =
    Show[List[A]].contramap(_.toList)

  "reverse" ! prop{ e: EphemeralStream[Int] =>
    e.reverse.toList must be_===(e.toList.reverse)
    e.reverse.reverse must be_===(e)
  }

  "foldLeft large stream" in {
    val list = List.fill(10000000)(1)
    val xs = EphemeralStream(list : _*)
    Foldable[EphemeralStream].foldLeft(xs, 0)(_ + _) must be_===(list.sum)
  }

  "foldLeft" ! prop{ xs: List[List[Int]] =>
    Foldable[EphemeralStream].foldLeft(EphemeralStream(xs: _*), List[Int]())(_ ::: _) must be_===(xs.foldLeft(List[Int]())(_ ::: _))
  }

  "unzip zip" ! prop { xs: EphemeralStream[(Int, Int)] =>
    val (firsts, seconds) = xs.unzip
    (firsts zip seconds) must be_===(xs)
  }

  "zip has right length" ! prop {(xs: EphemeralStream[Int], ys: EphemeralStream[Int]) =>
    (xs zip ys).length must be_===(xs.length min ys.length)
  }

  "interleave has right length" ! prop {(xs: EphemeralStream[Int], ys: EphemeralStream[Int]) =>
    (xs interleave ys).length must be_===(xs.length + ys.length)
  }
}
