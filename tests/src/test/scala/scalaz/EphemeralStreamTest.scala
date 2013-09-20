package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._
import syntax.contravariant._

class EphemeralStreamTest extends Spec {

  checkAll(equal.laws[EphemeralStream[Int]])
  checkAll(monadPlus.laws[EphemeralStream])
  checkAll(traverse.laws[EphemeralStream])
  checkAll(zip.laws[EphemeralStream])
  checkAll(align.laws[EphemeralStream])
  checkAll(cobind.laws[EphemeralStream])

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

  "take" ! prop { (xs: Stream[Int], n: Int) =>
    EphemeralStream.fromStream(xs).take(n) must be_===(EphemeralStream.fromStream(xs.take(n)))
  }

  "take from infinite stream" in {
    val n = util.Random.nextInt(1000)
    EphemeralStream.iterate(0)(_ + 1).take(n) must be_===(EphemeralStream.fromStream(Stream.iterate(1)(_ + 1).take(n)))
  }

  "takeWhile" ! prop { (xs: Stream[Int], n: Int) =>
    EphemeralStream.fromStream(xs).takeWhile(_ < n) must be_===(EphemeralStream.fromStream(xs.takeWhile(_ < n)))
  }

  "takeWhile from infinite stream" in {
    val n = util.Random.nextInt(1000)
    EphemeralStream.iterate(0)(_ + 1).takeWhile(_ < n) must be_===(EphemeralStream.fromStream(Stream.iterate(1)(_ + 1).takeWhile(_ < n)))
  }

  "index" ! prop {(xs: EphemeralStream[Int], i: Int) =>
    Foldable[EphemeralStream].index(xs, i) must be_===(xs.toList.lift.apply(i))
  }

  "index infinite stream" in {
    val i = util.Random.nextInt(1000)
    val xs = Stream from 0
    Foldable[EphemeralStream].index(EphemeralStream.fromStream(xs), i) must be_===(xs.lift.apply(i))
  }

  "inits" ! prop { xs: EphemeralStream[Int] =>
    import syntax.std.list._
    xs.inits.map(_.toList).toList must be_===(xs.toList.initz)
  }

  "tails" ! prop { xs: EphemeralStream[Int] =>
    import syntax.std.list._
    xs.tails.map(_.toList).toList must be_===(xs.toList.tailz)
  }

  "inits infinite stream" in {
    EphemeralStream.iterate(0)(_ + 1).inits
    ok
  }

  "tails infinite stream" in {
    val n = util.Random.nextInt(1000)
    EphemeralStream.iterate(0)(_ + 1).tails.map(_.take(n).toStream).take(n) must be_===(
      EphemeralStream.fromStream(Stream.iterate(1)(_ + 1).tails.map(_ take n).toStream.take(n))
    )
  }

  "no stack overflow infinite stream foldMap" in {
    val infiniteStream = EphemeralStream.iterate(false)(identity)
    Foldable[EphemeralStream].foldMap(infiniteStream)(identity)(booleanInstance.conjunction) must be_===(false)
  }

  "no stack overflow infinite stream foldRight" in {
    val infiniteStream = EphemeralStream.iterate(true)(identity)
    Foldable[EphemeralStream].foldRight(infiniteStream, true)(_ || _) must be_===(true)
  }
}
