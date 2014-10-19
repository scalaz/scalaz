package scalaz

import std.AllInstances._
import syntax.foldable1._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.Prop.forAll

object Foldable1Test extends SpecLite {
  "maximum1" ! forAll {
    (xs: NonEmptyList[Int]) =>
      (xs.maximum1) must_===(xs.list.toList.max)
  }
  "maximumOf1" ! forAll {
    (xs: NonEmptyList[Int]) =>
      val f: Int => Double = 1D + _
      (xs maximumOf1 f) must_===((xs.list.toList.iterator map f).max)
  }
  "maximumBy1" ! forAll {
    (xs: NonEmptyList[Int]) =>
      val f: Int => String = _.toString
      (xs maximumBy1 f) must_===((xs.list zip (xs.list map f)).toList.maxBy(_._2)._1)
  }
  "minimum1" ! forAll {
    (xs: NonEmptyList[Int]) =>
      (xs.minimum1) must_===(xs.list.toList.min)
  }
  "minimumOf1" ! forAll {
    (xs: NonEmptyList[Int]) =>
      val f: Int => Double = 1D + _
      (xs minimumOf1 f) must_===((xs.list.toList.iterator map f).min)
  }
  "minimumBy1" ! forAll {
    (xs: NonEmptyList[Int]) =>
      val f: Int => String = _.toString
      (xs minimumBy1 f) must_===((xs.list zip (xs.list map f)).toList.minBy(_._2)._1)
  }

  private val L = Foldable1[NonEmptyList]

  "product foldRight1 equivalence" ! forAll {
    (l: NonEmptyList[IList[Int]], l2: NonEmptyList[IList[Int]]) =>
      (L.product(L).foldRight1((l, l2))(_ ++ _)
       must_===((l.list ++ l2.list).flatten))
  }

  "product foldLeft1 equivalence" ! forAll {
    (l: NonEmptyList[IList[Int]], l2: NonEmptyList[IList[Int]]) =>
      (L.product(L).foldLeft1((l, l2))((xs, x) => x ++ xs)
       must_===((l.list ++ l2.list).reverse.flatten))
  }

  "intercalate1" ! forAll {
    (l: NonEmptyList[IList[Int]], x: IList[Int]) =>
      l.intercalate1(x) must_=== Foldable[IList].intercalate(l.list, x)
  }

  "toNel" ! forAll {
    intAnd: OneAnd[IList, Int] =>
    intAnd.toNel must_=== NonEmptyList.nel(intAnd.head, intAnd.tail)
  }
}
