package scalaz
package std

import org.scalacheck.Arbitrary

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import Id._
import org.scalacheck.Prop.forAll

object VectorTest extends SpecLite {
  implicit def vectorArb[A: Arbitrary] =
    Arbitrary(implicitly[Arbitrary[List[A]]].arbitrary map (_.toVector))

  import std.vector._
  checkAll(equal.laws[Vector[Int]])
  checkAll(monoid.laws[Vector[Int]])
  checkAll(monadPlus.strongLaws[Vector])
  checkAll(bindRec.laws[Vector])
  checkAll(traverse.laws[Vector])
  checkAll(FoldableTests.anyAndAllLazy[Vector])
  checkAll(zip.laws[Vector])
  checkAll(isEmpty.laws[Vector])
  checkAll(align.laws[Vector])

  import std.vector.vectorSyntax._
  import syntax.foldable._

  private def evenp(x: Int): Boolean = x % 2 == 0

  "filterM" ! forAll {
    (xs: Vector[Int]) => xs.filterM[Id](evenp) == xs.filter(_ % 2 == 0)
  }

  "filter consistent with fiterM[Id]" ! forAll {
    (xs: Vector[Int], p: Int => Boolean) => MonadPlus[Vector].filter(xs)(p) must_=== xs.filterM[Id](p)
  }

  "initz" ! forAll {
    (xs: Vector[Int]) =>
      initz(xs) must_===(xs.inits.toVector.reverse)
  }

  "tailz" ! forAll {
    (xs: Vector[Int]) => tailz(xs) must_===(xs.tails.toVector)
  }

  "spanM" ! forAll {
    (xs: Vector[Int]) =>
      (xs.spanM[Id](evenp)
       must_===(xs.takeWhile(evenp) -> xs.dropWhile(evenp)))
  }

  "takeWhileM" ! forAll {
    (xs: Vector[Int]) =>
      takeWhileM[Int, Id](xs)(evenp) must_===(xs takeWhile evenp)
  }

  "groupWhen" ! forAll {
    (xs: Vector[Int]) =>
      (xs.groupWhen(_ < _)
       must_===(list.groupWhen(xs.toList)(_ < _)
                   .map(_.toVector).toVector))
  }

  "partitionM" ! forAll {
    (xs: Vector[Int]) =>
      val (evens, odds) = xs.partitionM[Id](evenp)
      (evens.toSet & odds.toSet) must_===(Set[Int]())
      (evens.filter(evenp) ++
       odds.filter(i => !evenp(i))).toSet must_===(xs.toSet)
  }

  "findM" ! forAll {
    (xs: Vector[Int]) =>
      val i = xs indexWhere evenp
      type W[A] = Writer[Vector[Int], A]
      val wxs = findM[Int, W](xs)(x =>
        WriterT.writer(Vector(x) -> evenp(x)))
      (wxs.written, wxs.value) must_==={
        if (i < 0) (xs, None)
        else (xs take (i+1), Some(xs(i)))
      }
  }

  "mapAccumLeft" ! forAll {
    (xs: Vector[Int]) =>
      mapAccumLeft(xs)(Vector[Int](), (c: Vector[Int], a) =>
        (c :+ a, a)) must_===(xs, xs)
  }

  "mapAccumRight" ! forAll {
    (xs: Vector[Int]) =>
      mapAccumRight(xs)(Vector[Int](), (c: Vector[Int], a) =>
        (c :+ a, a)) must_===(xs.reverse, xs)
  }

  "Issue #266" in {
    import syntax.std.list._
    List(1, 2, 4).groupWhen((i1, i2) => scala.math.abs(i1 - i2) <= 1).length must_===(2)
    List(1, 2, 4).toVector.groupWhen((i1, i2) => scala.math.abs(i1 - i2) <= 1).length must_===(2)
  }

  "index" ! forAll { (xs: Vector[Int], n: Int) =>
    (xs index n) must_===(if (n >= 0 && xs.size > n) Some(xs(n)) else None)
  }

  "groupWhen is groupWhenM[Id]" ! forAll { xs: Vector[Int] =>
    val f: (Int, Int) => Boolean = _ > _
    xs.groupWhen(f) must_=== xs.groupWhenM[Id.Id](f)
  }

  object instances {
    def equal[A: Equal] = Equal[Vector[A]]
    def order[A: Order] = Order[Vector[A]]
    def semigroup[A: Semigroup] = Monoid[Vector[A]]
    def bindRec = BindRec[Vector]
    def monadPlus = MonadPlus[Vector]
    def traverse = Traverse[Vector]
    def zip = Zip[Vector]
    def unzip = Unzip[Vector]
    def align = Align[Vector]
    def isEmpty = IsEmpty[Vector]
  }
}
