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
}
