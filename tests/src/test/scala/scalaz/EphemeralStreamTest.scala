package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._
import syntax.contravariant._
import org.scalacheck.Prop.forAll

object EphemeralStreamTest extends SpecLite {

  checkAll(equal.laws[EphemeralStream[Int]])
  checkAll(bindRec.laws[EphemeralStream])
  checkAll(monadPlus.strongLaws[EphemeralStream])
  checkAll(isEmpty.laws[EphemeralStream])
  checkAll(traverse.laws[EphemeralStream])
  checkAll(zip.laws[EphemeralStream])
  checkAll(align.laws[EphemeralStream])
  checkAll(cobind.laws[EphemeralStream])
  checkAll(alt.laws[EphemeralStream])
  checkAll(monoid.laws[EphemeralStream[Int]])
  checkAll(order.laws[EphemeralStream[Int]])

  implicit def ephemeralStreamShow[A: Show]: Show[EphemeralStream[A]] =
    Show[List[A]].contramap(_.toList)

  import EphemeralStream.EStream

  "Order[EStream[Int]] is consistent with Order[List[Int]]" ! forAll {
    (a: EStream[Int], b: EStream[Int]) =>
      Order[EStream[Int]].order(a, b) must_=== Order[List[Int]].order(a.toList, b.toList)
  }

  "Order[EStream[Int]] is lazy" ! {
    var evaluated = false
    val a = 1 ##:: {evaluated = true; 2} ##:: EphemeralStream.emptyEphemeralStream[Int]
    val b = 0 ##:: EphemeralStream.emptyEphemeralStream[Int]
    Order[EStream[Int]].order(a, b) must_=== Ordering.GT
    Order[EStream[Int]].order(b, a) must_=== Ordering.LT
    evaluated must_=== false
  }

  "reverse" ! forAll{ (e: EphemeralStream[Int]) =>
    e.reverse.toList must_===(e.toList.reverse)
    e.reverse.reverse must_===(e)
  }

  "foldLeft" ! forAll{ (xs: List[List[Int]]) =>
    Foldable[EphemeralStream].foldLeft(EphemeralStream(xs *), List[Int]())(_ ::: _) must_===(xs.foldLeft(List[Int]())(_ ::: _))
  }

  // https://github.com/scalaz/scalaz/pull/1151
  "issue 1151" ! {
    def oldFoldLeft[A, B](a: EphemeralStream[A])(z: => B)(f: (=> B) => (=> A) => B): B = {
      var t = a
      var acc = z
      while (!t.isEmpty) {
        acc = f(acc)(t.head())
        t = t.tail()
      }
      acc
    }

    val es = EphemeralStream.unfold(0)(n => if (n <= 10) Some((n, n + 1)) else None)

    try {
      oldFoldLeft(es)(Need(0))(x => y => Need(y)).value
      throw new Throwable("fail")
    } catch {
      case ex: RuntimeException =>
        ex.getMessage must_=== "head of empty stream"
    }

    es.foldLeft(Need(0))((x, y) => Need(y)).value must_=== 10
  }

  "unzip zip" ! forAll { (xs: EphemeralStream[(Int, Int)]) =>
    val (firsts, seconds) = xs.unzip
    (firsts zip seconds) must_===(xs)
  }

  "zip has right length" ! forAll {(xs: EphemeralStream[Int], ys: EphemeralStream[Int]) =>
    (xs zip ys).length must_===(xs.length min ys.length)
  }

  "interleave has right length" ! forAll {(xs: EphemeralStream[Int], ys: EphemeralStream[Int]) =>
    (xs interleave ys).length must_===(xs.length + ys.length)
  }

  "take" ! forAll { (xs: LazyList[Int], n: Int) =>
    EphemeralStream.fromLazyList(xs).take(n) must_===(EphemeralStream.fromLazyList(xs.take(n)))
  }

  "take from infinite LazyList" in {
    val n = util.Random.nextInt(1000)
    EphemeralStream.iterate(0)(_ + 1).take(n) must_===(EphemeralStream.fromLazyList(LazyList.iterate(0)(_ + 1).take(n)))
  }

  "takeWhile" ! forAll { (xs: LazyList[Int], n: Int) =>
    EphemeralStream.fromLazyList(xs).takeWhile(_ < n) must_===(EphemeralStream.fromLazyList(xs.takeWhile(_ < n)))
  }

  "takeWhile from infinite LazyList" in {
    val n = util.Random.nextInt(1000)
    EphemeralStream.iterate(0)(_ + 1).takeWhile(_ < n) must_===(EphemeralStream.fromLazyList(LazyList.iterate(0)(_ + 1).takeWhile(_ < n)))
  }

  "index" ! forAll {(xs: EphemeralStream[Int], i: Int) =>
    Foldable[EphemeralStream].index(xs, i) must_===(xs.toList.lift.apply(i))
  }

  "index infinite LazyList" in {
    val i = util.Random.nextInt(1000)
    val xs = LazyList from 0
    Foldable[EphemeralStream].index(EphemeralStream.fromLazyList(xs), i) must_===(xs.lift.apply(i))
  }

  "inits" ! forAll { (xs: EphemeralStream[Int]) =>
    import syntax.std.list._
    xs.inits.map(_.toList).toList must_===(xs.toList.initz)
  }

  "tails" ! forAll { (xs: EphemeralStream[Int]) =>
    import syntax.std.list._
    xs.tails.map(_.toList).toList must_===(xs.toList.tailz)
  }

  "inits infinite stream" in {
    EphemeralStream.iterate(0)(_ + 1).inits
    ()
  }

  "tails infinite stream" in {
    val n = util.Random.nextInt(1000)
    EphemeralStream.iterate(0)(_ + 1).tails
      .map(t => Foldable[EphemeralStream].toLazyList(t.take(n)))
      .take(n) must_===(
      EphemeralStream.fromLazyList(LazyList.iterate(0)(_ + 1).tails.map(_ take n).take(n).to(LazyList))
    )
  }

  "foldMap evaluates lazily" in {
    val infiniteStream = EphemeralStream.iterate(false)(identity)
    Foldable[EphemeralStream].foldMap(infiniteStream)(identity)(using booleanInstance.conjunction) must_===(false)
  }

  "foldMap1Opt identity" ! forAll {
    (xs: EphemeralStream[Int]) =>
    Foldable[EphemeralStream].foldMap1Opt(xs)(Vector(_)).getOrElse(Vector.empty) must_===(Foldable[EphemeralStream].toVector(xs))
  }

  "foldMap1Opt evaluates lazily" in {
    val infiniteStream = EphemeralStream.iterate(false)(identity)
    Foldable[EphemeralStream].foldMap1Opt(infiniteStream)(identity)(using booleanInstance.conjunction) must_===(Some(false))
  }

  "foldRight evaluates lazily" in {
    val infiniteStream = EphemeralStream.iterate(true)(identity)
    Foldable[EphemeralStream].foldRight(infiniteStream, true)(_ || _) must_===(true)
  }

  "foldMapLeft1Opt identity" ! forAll {
    (xs: EphemeralStream[Int]) =>
    Foldable[EphemeralStream].foldMapLeft1Opt(xs.reverse)(EphemeralStream(_))((xs, x) => x ##:: xs) must_===(
      if (xs.isEmpty) None else Some(xs)
    )
  }

  "foldMapRight1Opt identity" ! forAll {
    (xs: EphemeralStream[Int]) =>
    Foldable[EphemeralStream].foldMapRight1Opt(xs)(EphemeralStream(_))(_ ##:: _) must_===(
      if (xs.isEmpty) None else Some(xs)
    )
  }

  "foldMapRight1Opt evaluates lazily" in {
    val infiniteStream = EphemeralStream.iterate(true)(identity)
    Foldable[EphemeralStream].foldMapRight1Opt(infiniteStream)(identity)(_ || _) must_===(Some(true))
  }

  "zipL" in {
    val size = 100
    val infinite = EphemeralStream.iterate(0)(_ + 1)
    val finite = EphemeralStream.range(0, size)
    val F = Traverse[EphemeralStream]
    F.zipL(infinite, infinite)
    F.zipL(finite, infinite).length must_===(size)
    F.zipL(finite, infinite) must_===((finite zip infinite).map{x => (x._1, Option(x._2))})
    F.zipL(infinite, finite).take(1000).length must_===(1000)
    F.zipL(infinite, finite).takeWhile(_._2.isDefined).length must_===(size)
  }

  "zipWithIndex" ! forAll { (xs: LazyList[Int], n: Int) =>
    EphemeralStream.fromLazyList(xs).take(n).zipWithIndex must_===(EphemeralStream.fromLazyList(xs.take(n).zipWithIndex))
  }

  "zipWithIndex from infinite LazyList" in {
    val n = util.Random.nextInt(1000)
    EphemeralStream.iterate(0)(_ + 1).zipWithIndex.take(n) must_===(
      EphemeralStream.fromLazyList(LazyList.iterate(0)(_ + 1).zipWithIndex.take(n))
    )
  }

  object instances {
    def equal[A: Equal] = Equal[EphemeralStream[A]]
    def order[A: Order] = Order[EphemeralStream[A]]
    def semigroup[A] = Semigroup[EphemeralStream[A]]
    def monoid[A] = Monoid[EphemeralStream[A]]

    // checking absence of ambiguity
    def equal[A: Order] = Equal[EphemeralStream[A]]
  }
}
