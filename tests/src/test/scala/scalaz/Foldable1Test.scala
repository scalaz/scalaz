package scalaz

import std.AllInstances._
import syntax.foldable1._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.Prop.forAll

object Foldable1Test extends SpecLite {
  "maximum1" ! forAll {
    (xs: NonEmptyList[Int]) =>
      (xs.maximum1) must_===(xs.list.max)
  }
  "maximumOf1" ! forAll {
    (xs: NonEmptyList[Int]) =>
      val f: Int => Double = 1D + _
      (xs maximumOf1 f) must_===((xs.list.iterator map f).max)
  }
  "maximumBy1" ! forAll {
    (xs: NonEmptyList[Int]) =>
      val f: Int => String = _.toString
      (xs maximumBy1 f) must_===((xs.list zip (xs.list map f)).maxBy(_._2)._1)
  }
  "minimum1" ! forAll {
    (xs: NonEmptyList[Int]) =>
      (xs.minimum1) must_===(xs.list.min)
  }
  "minimumOf1" ! forAll {
    (xs: NonEmptyList[Int]) =>
      val f: Int => Double = 1D + _
      (xs minimumOf1 f) must_===((xs.list.iterator map f).min)
  }
  "minimumBy1" ! forAll {
    (xs: NonEmptyList[Int]) =>
      val f: Int => String = _.toString
      (xs minimumBy1 f) must_===((xs.list zip (xs.list map f)).minBy(_._2)._1)
  }

  "distinct1" ! forAll {
    (xs: NonEmptyList[Int]) =>
      xs.distinct1.toList must_== xs.toList.distinct
      xs.distinct1(Order.order((_,_) => Ordering.EQ)).length must_== 1
  }

  "distinctE1" ! forAll {
    (xs: NonEmptyList[Int]) =>
      xs.distinctE1.toList must_== xs.toList.distinct
      xs.distinctE1(Equal.equal((_,_) => true)).length must_== 1
  }

  private val L = Foldable1[NonEmptyList]

  "product foldRight1 equivalence" ! forAll {
    (l: NonEmptyList[List[Int]], l2: NonEmptyList[List[Int]]) =>
      (L.product(L).foldRight1((l, l2))(_ ++ _)
       must_===((l.list ++ l2.list).flatten))
  }

  "product foldLeft1 equivalence" ! forAll {
    (l: NonEmptyList[List[Int]], l2: NonEmptyList[List[Int]]) =>
      (L.product(L).foldLeft1((l, l2))((xs, x) => x ++ xs)
       must_===((l.list ++ l2.list).reverse.flatten))
  }

  "intercalate1" ! forAll {
    (l: NonEmptyList[List[Int]], x: List[Int]) =>
      l.intercalate1(x) must_=== Foldable[List].intercalate(l.list, x)
  }
}
