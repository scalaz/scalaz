package scalaz

import std.AllInstances._
import syntax.foldable1._
import scalaz.scalacheck.ScalazProperties._
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
  "foldMap1Opt in Foldable" ! prop {
      (xs: NonEmptyList[String]) =>
      val F = Foldable1[NonEmptyList]
      val strlen = (_ : String).length
      import syntax.foldable._
      (xs foldMap1Opt strlen) must be_===(Some(F.foldMap1(xs)(strlen)): Option[Int])
  }
  "foldMap1Opt in Foldable1" ! prop {
      (xs: NonEmptyList[String]) =>
      val F = Foldable1[NonEmptyList]
      val strlen = (_ : String).length
      import syntax.foldable1._
      (xs foldMap1Opt strlen) must be_===(Some(F.foldMap1(xs)(strlen)): Option[Int])
  }
  "foldLeft1Opt in Foldable" ! prop {
      (xs: NonEmptyList[Int]) =>
      val F = Foldable1[NonEmptyList]
      val gt = (i : Int, j : Int) => if (i > j ) i else j
      import syntax.foldable._
      (xs foldLeft1Opt gt) must be_===(Some(F.foldLeft1(xs)(gt)): Option[Int])
  }
  "foldLeft1Opt in Foldable1" ! prop {
      (xs: NonEmptyList[Int]) =>
      val F = Foldable1[NonEmptyList]
      val gt = (i : Int, j : Int) => if (i > j ) i else j
      import syntax.foldable1._
      (xs foldLeft1Opt gt) must be_===(Some(F.foldLeft1(xs)(gt)): Option[Int])
  }
  "foldRight1Opt in Foldable" ! prop {
      (xxs: NonEmptyList[Int]) =>
      val F = Foldable1[NonEmptyList]
      val gt: (Int, => Int) => Int = (i, j) => if (i > 0) i else j
      import syntax.foldable._
      (xxs foldRight1Opt gt) must be_===(Some(F.foldRight1(xxs)(gt)): Option[Int])
  }
  "foldRight1Opt in Foldable1" ! prop {
      (xxs: NonEmptyList[Int]) =>
      val F = Foldable1[NonEmptyList]
      val gt: (Int, => Int) => Int = (i, j) => if (i > 0) i else j
      import syntax.foldable1._
      (xxs foldRight1Opt gt) must be_===(Some(F.foldRight1(xxs)(gt)): Option[Int])
  }
  
  "foldr1Opt in Foldable" ! prop {
      (xxs: NonEmptyList[Int]) =>
      val F = Foldable1[NonEmptyList]
      val gt = (i: Int, j: Int) => if (i > j ) i else j
      import syntax.foldable._
      (xxs foldr1Opt (i => (j => gt(i, j)))) must be_===(Some(F.foldr1(xxs)((i => (j => gt(i, j))))): Option[Int])
  }
  "foldr1Opt in Foldable1" ! prop {
      (xxs: NonEmptyList[Int]) =>
      val F = Foldable1[NonEmptyList]
      val gt = (i: Int, j: Int) => if (i > j ) i else j
      import syntax.foldable1._
      (xxs foldr1Opt (i => (j => gt(i, j)))) must be_===(Some(F.foldr1(xxs)((i => (j => gt(i, j))))): Option[Int])
  }
  "foldl1Opt in Foldable" ! prop {
      (xs: NonEmptyList[Int]) =>
      val F = Foldable1[NonEmptyList]
      val gt = (i: Int) => (j: Int) => if (i > j ) i else j
      import syntax.foldable._
      (xs foldl1Opt gt) must be_===(Some(F.foldl1(xs)(gt)): Option[Int])
  }
  "foldl1Opt in Foldable1" ! prop {
      (xs: NonEmptyList[Int]) =>
      val F = Foldable1[NonEmptyList]
      val gt = (i: Int) => (j: Int) => if (i > j ) i else j
      import syntax.foldable1._
      (xs foldl1Opt gt) must be_===(Some(F.foldl1(xs)(gt)): Option[Int])
  }

}
