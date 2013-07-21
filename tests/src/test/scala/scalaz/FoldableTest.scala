package scalaz

import std.AllInstances._
import syntax.foldable._

import org.specs2.matcher._

class FoldableTest extends Spec with OptionMatchers {
  "to" ! prop {
    (xs: List[Int]) =>
      val v: Vector[Int] = Foldable[List].to[Int, Vector](xs)
      v.toList must_== xs
  }
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

  "non-empty folding" should {

    val gt1: (Int, Int)    => Int = (i, j) => i - j
    val gt2: (Int, => Int) => Int = (i, j) => i - j
    val strlen = (_ : String).length

    import syntax.foldable1._
    import syntax.std.list._

    "foldLeft1Opt" ! prop {
      (xs: List[Int]) =>
        xs match {
          case Nil     => (xs foldLeft1Opt gt1) must beNone
          case y :: ys => (xs foldLeft1Opt gt1) must beSome(ys.foldLeft(y)(gt1))
        }
    }

    "foldRight1Opt" ! prop {
      (xs: List[Int]) =>
        xs match {
          case Nil => (xs foldRight1Opt gt2) must beNone
          case _   => (xs foldRight1Opt gt2) must beSome(xs.init.foldRight(xs.last)(gt1))
        }
    }

    "foldl1Opt" ! prop {
      (xs: List[Int]) =>
        xs match {
          case Nil     => (xs foldl1Opt gt1.curried) must beNone
          case y :: ys => (xs foldl1Opt gt1.curried) must beSome(ys.foldLeft(y)(gt1))
        }
    }

    "foldr1Opt" ! prop {
      (xs: List[Int]) =>
        xs match {
          case Nil => (xs foldr1Opt gt2.curried) must beNone
          case _   => (xs foldr1Opt gt2.curried) must beSome(xs.init.foldRight(xs.last)(gt1))
        }
    }

    "foldMap1Opt" ! prop {
      (xs: List[String]) =>
        xs.toNel match {
          case None      => (xs foldMap1Opt strlen) must beNone
          case Some(nel) => (xs foldMap1Opt strlen) must beSome(nel.foldMap1(strlen))
        }
    }

  }

  private val L = Foldable[List]

  "product foldRight equivalence" ! prop {
    (l: List[Int], l2: List[Int]) =>
      L.product(L).foldRight((l, l2), List.empty[Int])(_ :: _) must be_===(l ++ l2)
  }

  "product foldLeft equivalence" ! prop {
    (l: List[Int], l2: List[Int]) =>
      (L.product(L).foldLeft((l, l2), List.empty[Int])((xs, x) => x :: xs)
       must be_===((l ++ l2).reverse))
  }
}
