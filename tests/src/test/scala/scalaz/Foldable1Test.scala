package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class Foldable1Test extends Spec {
  "maximum" ! prop {
    (xs: NonEmptyList[Int]) =>
      val F = Foldable1[NonEmptyList]
     (F maximum1 xs) must be_===(xs.list.max)
  }
  "maximumOf1" ! prop {
    (xs: NonEmptyList[Int]) =>
      val F = Foldable1[NonEmptyList]
	  val f: Int => Double = 1D + _
     (F.maximumOf1(xs)(f)) must be_===((xs.list.iterator map f).max)
  }
  "maximumBy" ! prop {
    (xs: NonEmptyList[Int]) =>
      val F = Foldable1[NonEmptyList]
	  val f: Int => String = _.toString
      (F.maximumBy1(xs)(f)) must be_===((xs.list zip (xs.list map f)).maxBy(_._2)._1)
  }
  "minimum" ! prop {
    (xs: NonEmptyList[Int]) =>
      val F = Foldable1[NonEmptyList]
     (F minimum1 xs) must be_===(xs.list.min)
  }
  "minimumOf1" ! prop {
    (xs: NonEmptyList[Int]) =>
      val F = Foldable1[NonEmptyList]
	  val f: Int => Double = 1D + _
     (F.minimumOf1(xs)(f)) must be_===((xs.list.iterator map f).min)
  }
  "minimumBy" ! prop {
    (xs: NonEmptyList[Int]) =>
      val F = Foldable1[NonEmptyList]
	  val f: Int => String = _.toString
      (F.minimumBy1(xs)(f)) must be_===((xs.list zip (xs.list map f)).minBy(_._2)._1)
  }
}
