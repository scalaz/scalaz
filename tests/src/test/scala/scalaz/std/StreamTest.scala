package scalaz
package std

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll

object StreamTest extends SpecLite {
  checkAll(equal.laws[Stream[Int]])
  checkAll(monoid.laws[Stream[Int]])
  checkAll(monadPlus.strongLaws[Stream])
  checkAll(traverse.laws[Stream])
  checkAll(cobind.laws[Stream])
  checkAll(isEmpty.laws[Stream])
  checkAll(zip.laws[Stream])
  checkAll(align.laws[Stream])

  import std.stream.streamSyntax._
  import syntax.foldable._

  "intercalate empty stream is flatten" ! forAll((a: Stream[Stream[Int]]) => a.intercalate(Stream.empty[Int]) must_===(a.flatten))

  "intersperse then remove odd items is identity" ! forAll {
    (a: Stream[Int], b: Int) =>
      val isEven = (_: Int) % 2 == 0
      a.intersperse(b).zipWithIndex.filter(p => isEven(p._2)).map(_._1) must_===(a)
  }

  "intercalate is same as intersperse(s).flatten" ! forAll {
    (a: Stream[Stream[Int]], b: Stream[Int]) =>
      a.intercalate(b) must_===(a.intersperse(b).flatten)
  }

  "intersperse vs benchmark" ! forAll {
    def intersperse[A](as: Stream[A], a: A): Stream[A] = {
      def loop(rest: Stream[A]): Stream[A] = rest match {
        case Stream.Empty => Stream.empty
        case h #:: t      => a #:: h #:: loop(t)
      }
      as match {
        case Stream.Empty => Stream.empty
        case h #:: t      => h #:: loop(t)
      }
    }
    (a: Stream[Int], b: Int) => (a.intersperse(b) must_===(intersperse(a, b)))
  }


  "foldl is foldLeft" ! forAll {(rnge: Stream[List[Int]]) =>
    val F = Foldable[Stream]
    (rnge.foldLeft(List[Int]())(_++_)
      must_===(F.foldLeft(rnge, List[Int]())(_++_)))
  }

  "foldr is foldRight" ! forAll {(rnge: Stream[List[Int]]) =>
    val F = Foldable[Stream]
    (rnge.foldRight(List[Int]())(_++_)
      must_===(F.foldRight(rnge, List[Int]())(_++_)))
  }

  "foldMap evaluates lazily" in {
    Foldable[Stream].foldMap(Stream.continually(false))(identity)(booleanInstance.conjunction) must_===(false)
  }

  "foldRight evaluates lazily" in {
    Foldable[Stream].foldRight(Stream.continually(true), true)(_ || _) must_===(true)
  }

  "zipL" in {
    val size = 100
    val infinite = Stream.from(1)
    val finite = Stream.range(0, size)
    val F = Traverse[Stream]
    F.zipL(infinite, infinite)
    F.zipL(finite, infinite).length must_===(size)
    F.zipL(finite, infinite) must_===((finite zip infinite).map{x => (x._1, Option(x._2))})
    F.zipL(infinite, finite).take(1000).length must_===(1000)
    F.zipL(infinite, finite).takeWhile(_._2.isDefined).length must_===(size)
  }

  "filter" ! forAll {
    (xs: Stream[Int], p: Int => Boolean) => MonadPlus[Stream].filter(xs)(p) must_=== xs.filter(p)
  }
}
