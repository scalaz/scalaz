package scalaz
package std

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._

class StreamTest extends Spec {
  checkAll(equal.laws[Stream[Int]])
  checkAll(monoid.laws[Stream[Int]])
  checkAll(monadPlus.strongLaws[Stream])
  checkAll(traverse.laws[Stream])

  import std.stream.streamSyntax._

  "intercalate empty stream is flatten" ! check((a: Stream[Stream[Int]]) => a.intercalate(Stream.empty[Int]) must be_===(a.flatten))

  "intersperse then remove odd items is identity" ! check {
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


  "foldl1 is reduceLeft" ! check {(hd: List[Int], rngel: Stream[List[Int]]) =>
    val rnge = hd #:: rngel
    val F = Foldable[Stream]
    Some(rnge.reduceLeft(_++_)) must be_===(F.foldl1(rnge)(_++_))
  }

  "foldl is foldLeft" ! check {(rnge: Stream[List[Int]]) =>
    val F = Foldable[Stream]
    (rnge.foldLeft(List[Int]())(_++_)
      must be_===(F.foldLeft(rnge, List[Int]())(_++_)))
  }

  "foldr1 is reduceRight" ! check {(hd: List[Int], rngel: Stream[List[Int]]) =>
    val rnge = hd #:: rngel
    val F = Foldable[Stream]
    Some(rnge.reduceRight(_++_)) must be_===(F.foldr1(rnge)(_++_))
  }

  "foldr is foldRight" ! check {(rnge: Stream[List[Int]]) =>
    val F = Foldable[Stream]
    (rnge.foldRight(List[Int]())(_++_)
      must be_===(F.foldRight(rnge, List[Int]())(_++_)))
  }
}
