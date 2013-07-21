package scalaz

import std.AllInstances._
import syntax.foldable1._
import scalaz.scalacheck.ScalazArbitrary._

class Foldable1Test extends Spec {
  "maximum1" ! prop {
    (xs: NonEmptyList[Int]) =>
      (xs.maximum1) must be_===(xs.list.max)
  }
  "maximumOf1" ! prop {
    (xs: NonEmptyList[Int]) =>
      val f: Int => Double = 1D + _
      (xs maximumOf1 f) must be_===((xs.list.iterator map f).max)
  }
  "maximumBy1" ! prop {
    (xs: NonEmptyList[Int]) =>
      val f: Int => String = _.toString
      (xs maximumBy1 f) must be_===((xs.list zip (xs.list map f)).maxBy(_._2)._1)
  }
  "minimum1" ! prop {
    (xs: NonEmptyList[Int]) =>
      (xs.minimum1) must be_===(xs.list.min)
  }
  "minimumOf1" ! prop {
    (xs: NonEmptyList[Int]) =>
      val f: Int => Double = 1D + _
      (xs minimumOf1 f) must be_===((xs.list.iterator map f).min)
  }
  "minimumBy1" ! prop {
    (xs: NonEmptyList[Int]) =>
      val f: Int => String = _.toString
      (xs minimumBy1 f) must be_===((xs.list zip (xs.list map f)).minBy(_._2)._1)
  }

  private val L = Foldable1[NonEmptyList]

  "product foldRight1 equivalence" ! prop {
    (l: NonEmptyList[List[Int]], l2: NonEmptyList[List[Int]]) =>
      (L.product(L).foldRight1((l, l2))(_ ++ _)
       must be_===((l.list ++ l2.list).flatten))
  }

  "product foldLeft1 equivalence" ! prop {
    (l: NonEmptyList[List[Int]], l2: NonEmptyList[List[Int]]) =>
      (L.product(L).foldLeft1((l, l2))((xs, x) => x ++ xs)
       must be_===((l.list ++ l2.list).reverse.flatten))
  }
}
