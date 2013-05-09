package scalaz

import std.AllInstances._
import syntax.foldable._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

import org.specs2.matcher._

class FoldableTest extends Spec with OptionMatchers {
  "maximum" ! prop {
    (xs: List[Int]) =>
	    if (xs.isEmpty)
        (xs.maximum) must beNone
      else
        (xs.maximum) must beSome(xs.max)
  }
  "maximumOf" ! prop {
    (xs: List[Int]) =>
      val f: Int => Double = 1D + _
      if (xs.isEmpty)
        (xs maximumOf f) must beNone
      else
        (xs maximumOf f) must beSome((xs.iterator map f).max)
  }
  "maximumBy" ! prop {
    (xs: List[Int]) =>
      val f: Int => String = _.toString
      if (xs.isEmpty)
        (xs maximumBy f) must beNone
      else
        (xs maximumBy f) must beSome((xs zip (xs map f)).maxBy(_._2)._1)
  }
  "minimum" ! prop {
    (xs: List[Int]) =>
	    if (xs.isEmpty)
        (xs.minimum) must beNone
      else
        (xs.minimum) must beSome(xs.min)
  }
  "minimumOf" ! prop {
    (xs: List[Int]) =>
      val f: Int => Double = 1D + _
      if (xs.isEmpty)
        (xs minimumOf f) must beNone
      else
        (xs minimumOf f) must beSome((xs.iterator map f).min)
  }
  "minimumBy" ! prop {
    (xs: List[Int]) =>
      val f: Int => String = _.toString
      if (xs.isEmpty)
        (xs minimumBy f) must beNone
      else
        (xs minimumBy f) must beSome((xs zip (xs map f)).minBy(_._2)._1)
  }
  
  "foldLeft1Opt" ! prop {
      (xs: List[Int]) =>
      val F = Foldable1[NonEmptyList]
      val gt = (i : Int, j : Int) => if (i > j ) i else j
      import syntax.foldable._
      import syntax.std.list._
      xs.toNel match {
        case None      => (xs foldLeft1Opt gt) must be_===(None: Option[Int])
        case Some(nel) => (xs foldLeft1Opt gt) must be_===(Some(F.foldLeft1(nel)(gt)): Option[Int])
      }

  }
  "foldRight1Opt" ! prop {
      (xxs: Stream[Int]) =>
      val F = Foldable[Stream]
      val gt: (Int, => Int) => Int = (i, j) => if (i > 0) i else j
      import syntax.foldable._
      import syntax.std.stream._
      xxs match {
        case Stream.Empty => (xxs foldRight1Opt gt) must be_===(None: Option[Int])
        case _            => (xxs foldRight1Opt gt) must be_===(Some(F.foldRight(xxs.init, xxs.last)(gt)): Option[Int])
      }
  }
  
  "foldr1Opt" ! prop {
      (xxs: Stream[Int]) =>
      val F = Foldable[Stream]
      val gt = (i: Int, j: Int) => if (i > j ) i else j
      import syntax.foldable._
      xxs match {
        case Stream.Empty => (xxs foldr1Opt (i => (j => gt(i, j)))) must be_===(None: Option[Int])
        case _            => (xxs foldr1Opt (i => (j => gt(i, j)))) must be_===(Some(F.foldr(xxs.init, xxs.last)((i => (j => gt(i, j))))): Option[Int])
      }
  }
  "foldl1Opt" ! prop {
      (xs: List[Int]) =>
      val F = Foldable1[NonEmptyList]
      val gt = (i: Int) => (j: Int) => if (i > j ) i else j
      import syntax.foldable._
      import syntax.std.list._
      xs.toNel match {
        case None      => (xs foldl1Opt gt) must be_===(None: Option[Int])
        case Some(nel) => (xs foldl1Opt gt) must be_===(Some(F.foldl1(nel)(gt)): Option[Int])
      }
  }
  
  "foldMap1Opt" ! prop {
      (xs: List[String]) =>
      val F = Foldable1[NonEmptyList]
      val strlen = (_ : String).length
      import syntax.foldable._
      import syntax.std.list._
      xs.toNel match {
        case None      => (xs foldMap1Opt strlen) must be_===(None: Option[Int])
        case Some(nel) => (xs foldMap1Opt strlen) must be_===(Some(F.foldMap1(nel)(strlen)): Option[Int])
      }
  }
}
