package scalaz

import std.AllInstances._
import syntax.foldable._
import syntax.equal._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Properties}

object FoldableTest extends SpecLite {
  "to" ! forAll {
    (xs: List[Int]) =>
      val v: Vector[Int] = Foldable[List].to[Int, Vector](xs)
      v.toList must_== xs
  }
  "maximum" ! forAll {
    (xs: List[Int]) =>
      if (xs.isEmpty)
        (xs.maximum) must_==(None)
      else
        (xs.maximum) must_== Some((xs.max))
  }
  "maximumOf" ! forAll {
    (xs: List[Int]) =>
      val f: Int => Double = 1D + _
      if (xs.isEmpty)
        (xs maximumOf f) must_==(None)
      else
        (xs maximumOf f) must_==(Some((xs.iterator map f).max))
  }
  "maximumBy" ! forAll {
    (xs: List[Int]) =>
      val f: Int => String = _.toString
      if (xs.isEmpty)
        (xs maximumBy f) must_== None
      else
        (xs maximumBy f) must_== Some((xs zip (xs map f)).maxBy(_._2)._1)
  }
  "minimum" ! forAll {
    (xs: List[Int]) =>
      if (xs.isEmpty)
        (xs.minimum) must_== None
      else
        (xs.minimum) must_== Some(xs.min)
  }
  "minimumOf" ! forAll {
    (xs: List[Int]) =>
      val f: Int => Double = 1D + _
      if (xs.isEmpty)
        (xs minimumOf f) must_== None
      else
        (xs minimumOf f) must_== Some((xs.iterator map f).min)
  }
  "minimumBy" ! forAll {
    (xs: List[Int]) =>
      val f: Int => String = _.toString
      if (xs.isEmpty)
        (xs minimumBy f) must_== None
      else
        (xs minimumBy f) must_== Some((xs zip (xs map f)).minBy(_._2)._1)
  }

  "distinct" ! forAll {
    (xs: List[Int]) =>
      val F = implicitly[Foldable[List]]
      F.distinct(xs).toList must_== xs.distinct
      if (xs.length > 0) F.distinct(xs)(Order.order((_,_) => Ordering.EQ)).length must_== 1
  }

  "distinctE" ! forAll {
    (xs: List[Int]) =>
      xs.distinctE.toList must_== xs.distinct
      if (xs.length > 0) xs.distinctE(Equal.equal((_,_) => true)).length must_== 1
  }

  "sumr1Opt" ! forAll {
    (xs: List[String]) => xs match {
      case Nil => xs.sumr1Opt must_== None
      case _ => xs.sumr1Opt must_== Some(xs.mkString)
    }
  }

  "suml1Opt" ! forAll {
    (xs: List[String]) => xs match {
      case Nil => xs.suml1Opt must_== None
      case _ => xs.suml1Opt must_== Some(xs.mkString)
    }
  }

  "non-empty folding" should {

    val gt1: (Int, Int)    => Int = (i, j) => i - j
    val gt2: (Int, => Int) => Int = (i, j) => i - j
    val strlen = (_ : String).length

    import syntax.foldable1._
    import syntax.std.list._

    "foldLeft1Opt" ! forAll {
      (xs: List[Int]) =>
        xs match {
          case Nil     => (xs foldLeft1Opt gt1) must_== None
          case y :: ys => (xs foldLeft1Opt gt1) must_== Some(ys.foldLeft(y)(gt1))
        }
    }

    "foldRight1Opt" ! forAll {
      (xs: List[Int]) =>
        xs match {
          case Nil => (xs foldRight1Opt gt2) must_== None
          case _   => (xs foldRight1Opt gt2) must_== Some(xs.init.foldRight(xs.last)(gt1))
        }
    }

    "foldl1Opt" ! forAll {
      (xs: List[Int]) =>
        xs match {
          case Nil     => (xs foldl1Opt gt1.curried) must_== None
          case y :: ys => (xs foldl1Opt gt1.curried) must_== Some(ys.foldLeft(y)(gt1))
        }
    }

    "foldr1Opt" ! forAll {
      (xs: List[Int]) =>
        xs match {
          case Nil => (xs foldr1Opt gt2.curried) must_== None
          case _   => (xs foldr1Opt gt2.curried) must_== Some(xs.init.foldRight(xs.last)(gt1))
        }
    }

    "foldMap1Opt" ! forAll {
      (xs: List[String]) =>
        xs.toNel match {
          case None      => (xs foldMap1Opt strlen) must_== None
          case Some(nel) => (xs foldMap1Opt strlen) must_== Some(nel.foldMap1(strlen))
        }
    }

    "fold1Opt" ! forAll {
      (xs: List[Int]) => xs.fold1Opt must_== xs.suml1Opt
    }

    "foldMapM" ! forAll {
      (xs: List[String]) => xs.foldMapM(x => Some(x): Option[String]) must_== Some(xs.mkString)
    }

    type StateInt[A] = State[Int, A]

    def found(z: Int): State[Int, Option[Int]] =
      State(n => (n + 1, Some(z * 2)))

    def notfound: State[Int, Option[Int]] =
      State(n => (n + 1, None))

    "findMapM: finding the first element performs transform and only runs only necessary effects" ! forAll {
      (x: Int, xs: List[Int]) => (x :: xs).findMapM[StateInt, Int](found).run(0) must_== (1 -> Some(x * 2))
    }

    "findMapM: finding the last element performs transform and runs all effects (once only)" ! forAll {
      (x: Int, xs: List[Int]) => !xs.contains(x) ==> {
        (xs ++ List(x)).findMapM[StateInt, Int](z => if (z == x) found(z) else notfound).run(0) must_==
          ((xs.length + 1) -> Some(x * 2))
      }
    }

    "findMapM: runs all effects but doesn't return a value for not found" ! forAll {
      (xs: List[Int]) => xs.findMapM[StateInt, Int](_ => notfound).run(0) must_== (xs.length -> None)
    }

    "findLeft" ! forAll {
      (x: Int, xs: List[Int]) => (x :: xs).findLeft(_ == x) must_== Some(x)
    }

    "findRight" ! forAll {
      (x: Int, xs: List[Int]) => (xs ++ List(x)).findRight(_ == x) must_== Some(x)
    }
  }

  private val L = Foldable[List]

  "product foldRight equivalence" ! forAll {
    (l: List[Int], l2: List[Int]) =>
      L.product(L).foldRight((l, l2), List.empty[Int])(_ :: _) must_===(l ++ l2)
  }

  "product foldLeft equivalence" ! forAll {
    (l: List[Int], l2: List[Int]) =>
      (L.product(L).foldLeft((l, l2), List.empty[Int])((xs, x) => x :: xs)
       must_===((l ++ l2).reverse))
  }
}

object FoldableTests {
  def anyIsLazy[F[_], A](implicit F: Foldable[F], arb: Arbitrary[F[A]]) = forAll { fa: F[A] =>
    var i = 0
    fa any { x =>
      i = i + 1
      true
    }
    val expected = if (fa.empty) 0 else 1
    i === expected
  }

  def allIsLazy[F[_], A](implicit F: Foldable[F], arb: Arbitrary[F[A]]) = forAll { fa: F[A] =>
    var i = 0
    fa all { x =>
      i = i + 1
      false
    }
    val expected = if (fa.empty) 0 else 1
    i === expected
  }

  def anyConsistent[F[_], A](f: A => Boolean)(implicit F: Foldable[F], fa: Arbitrary[F[A]]) =
    forAll { fa: F[A] =>
      F.any(fa)(f) === F.toList(fa).exists(f)
    }

  def allConsistent[F[_], A](f: A => Boolean)(implicit F: Foldable[F], fa: Arbitrary[F[A]]) =
    forAll { fa: F[A] =>
      F.all(fa)(f) === F.toList(fa).forall(f)
    }

  def anyAndAllLazy[F[_]](implicit fa: Arbitrary[F[Int]], F: Foldable[F]) = {
    val p = new Properties("foldable")
    p.property("consistent any") = anyConsistent[F, Int](_ > 0)
    p.property("consistent all") = allConsistent[F, Int](_ > 0)
    p.property("any is lazy") = anyIsLazy[F, Int]
    p.property("all is lazy") = allIsLazy[F, Int]
    p
  }
}
