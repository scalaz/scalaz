package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class FoldableTest extends Spec {
  "maximum" ! prop {
    (xs: List[Int]) =>
      val F = Foldable[List]
	  if (xs.isEmpty)
		(F maximum xs) must be_===(None: Option[Int])
	  else
	    (F maximum xs) must be_===(Some(xs.max): Option[Int])
  }
  "maximumOf" ! prop {
    (xs: List[Int]) =>
      val F = Foldable[List]
	  val f: Int => Double = 1D + _
	  if (xs.isEmpty)
		(F.maximumOf(xs)(f)) must be_===(None: Option[Double])
	  else
	    (F.maximumOf(xs)(f)) must be_===(Some( (xs.iterator map f).max ): Option[Double])
  }
  "maximumBy" ! prop {
    (xs: List[Int]) =>
      val F = Foldable[List]
	  val f: Int => String = _.toString
	  if (xs.isEmpty)
		(F.maximumBy(xs)(f)) must be_===(None: Option[Int])
	  else
	    (F.maximumBy(xs)(f)) must be_===(Some( (xs zip (xs map f)).maxBy(_._2)._1 ): Option[Int])
  }
  "minimum" ! prop {
    (xs: List[Int]) =>
      val F = Foldable[List]
	  if (xs.isEmpty)
		(F minimum xs) must be_===(None: Option[Int])
	  else
	    (F minimum xs) must be_===(Some(xs.min): Option[Int])
  }
  "minimumOf" ! prop {
    (xs: List[Int]) =>
      val F = Foldable[List]
	  val f: Int => Double = 1D + _
	  if (xs.isEmpty)
		(F.minimumOf(xs)(f)) must be_===(None: Option[Double])
	  else
	    (F.minimumOf(xs)(f)) must be_===(Some((xs.iterator map f).min): Option[Double])
  }
  "minimumBy" ! prop {
    (xs: List[Int]) =>
      val F = Foldable[List]
	  val f: Int => String = _.toString
	  if (xs.isEmpty)
		(F.minimumBy(xs)(f)) must be_===(None: Option[Int])
	  else
	    (F.minimumBy(xs)(f)) must be_===(Some( (xs zip (xs map f)).minBy(_._2)._1 ): Option[Int])
  }
}
