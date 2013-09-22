package scalaz
package std

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._

class StreamTest extends Spec {
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

  "intercalate empty stream is flatten" ! check((a: Stream[Stream[Int]]) => a.intercalate(Stream.empty[Int]) must be_===(a.flatten))

  "intersperse then remove odd items is identity" ! prop {
    (a: Stream[Int], b: Int) =>
      val isEven = (_: Int) % 2 == 0
      a.intersperse(b).zipWithIndex.filter(p => isEven(p._2)).map(_._1) must be_===(a)
  }

  "intercalate is same as intersperse(s).flatten" ! check  {
    (a: Stream[Stream[Int]], b: Stream[Int]) =>
      a.intercalate(b) must be_===(a.intersperse(b).flatten)
  }

  "intersperse vs benchmark" ! check  {
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
    (a: Stream[Int], b: Int) => (a.intersperse(b) must be_===(intersperse(a, b)))
  }


  "foldl is foldLeft" ! prop {(rnge: Stream[List[Int]]) =>
    val F = Foldable[Stream]
    (rnge.foldLeft(List[Int]())(_++_)
      must be_===(F.foldLeft(rnge, List[Int]())(_++_)))
  }

  "foldr is foldRight" ! prop {(rnge: Stream[List[Int]]) =>
    val F = Foldable[Stream]
    (rnge.foldRight(List[Int]())(_++_)
      must be_===(F.foldRight(rnge, List[Int]())(_++_)))
  }

  "no stack overflow infinite stream foldMap" in {
    Foldable[Stream].foldMap(Stream.continually(false))(identity)(booleanInstance.conjunction) must be_===(false)
  }

  "no stack overflow infinite stream foldRight" in {
    Foldable[Stream].foldRight(Stream.continually(true), true)(_ || _) must be_===(true)
  }

  "zipL" in {
    val size = 100
    val infinite = Stream.from(1)
    val finite = Stream.range(0, size)
    val F = Traverse[Stream]
    F.zipL(infinite, infinite)
    F.zipL(finite, infinite).length must be_===(size)
    F.zipL(finite, infinite) must be_===((finite zip infinite).map{x => (x._1, Option(x._2))})
    F.zipL(infinite, finite).take(1000).length must be_===(1000)
    F.zipL(infinite, finite).takeWhile(_._2.isDefined).length must be_===(size)
  }
}
