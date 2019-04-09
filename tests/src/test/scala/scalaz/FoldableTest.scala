package scalaz

import std.AllInstances._
import syntax.apply._
import syntax.foldable._
import syntax.equal._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Properties}
import scalaz.Maybe.{Empty, Just}
//import scalaz.Foldable.FromFoldMap

object FoldableTest extends SpecLite {
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
  "extrema" ! forAll {
    (xs: List[Int]) =>
      (xs.extrema) must_== xs.minimum.tuple(xs.maximum)
  }
  "extremaOf" ! forAll {
    (xs: List[Int]) =>
      val f: Int => Double = 1D + _
      (xs extremaOf f) must_== (xs minimumOf f).tuple(xs maximumOf f)
  }
  "extremaBy" ! forAll {
    (xs: List[Int], f: Int => Int) =>
      (xs extremaBy f) must_== (xs minimumBy f).tuple(xs maximumBy f)
  }
  "extremaBy consistent with minimumBy/maximumBy" ! {
    val xs = (1 to 6).toList
    val f: Int => Int = _ % 3
    (xs extremaBy f) must_== (xs minimumBy f).tuple(xs maximumBy f)

    val g: Int => Int = _ => 0
    (xs extremaBy g) must_== (xs minimumBy g).tuple(xs maximumBy g)
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

  "distinctBy" ! {
    case class Foo(a: Int, b: String)
    val xs = IList(Foo(1, "x"), Foo(2, "x"), Foo(1, "y"))
    xs.distinctBy(_.a) must_== IList(Foo(1, "x"), Foo(2, "x"))
    xs.distinctBy(_.b) must_== IList(Foo(1, "x"), Foo(1, "y"))
  }

  "sumr1Opt" ! forAll {
    (xs: List[String]) => xs match {
      case Nil => xs.sumr1Maybe must_== None
      case _ => xs.sumr1Maybe must_== Some(xs.mkString)
    }
  }

  "suml1Opt" ! forAll {
    (xs: List[String]) => xs match {
      case Nil => xs.suml1Maybe must_== None
      case _ => xs.suml1Maybe must_== Some(xs.mkString)
    }
  }

  "psum should be stack-safe and short-circuiting" in {
    import Maybe.{empty, just}
    val N = 10000
    Stream.from(1).map(i =>
      if(i < N)
        empty[String]
      else if(i < N+2)
        // put two "Stop" elements before "BOOM!",
        // because Stream always evaluates the first element
        just("Stop")
      else
        sys.error("BOOM!")
    ).psum must_=== just("Stop")
  }

  "psumMap should be stack-safe and short-circuiting with Stream" in {
    import Maybe.{empty, just}
    val N = 10000
    Stream.from(1).psumMap(i =>
      if(i < N) empty[String]
      else if(i == N) just("Stop")
      else sys.error("BOOM!")
    ) must_=== just("Stop")
  }

  "psumMap should be stack-safe and short-circuiting with EphemeralStream" in {
    import Maybe.{empty, just}
    val N = 10000
    val xs = EphemeralStream.fromStream(Stream.from(1))
    xs.psumMap(i =>
      if(i < N) empty[String]
      else if(i == N) just("Stop")
      else sys.error("BOOM!")
    ) must_=== just("Stop")
  }

  "psumMap should be stack-safe and short-circuiting with List" in {
    import Maybe.{empty, just}
    val N = 10000
    List.range(1, 11000).psumMap(i =>
      if(i < N) empty[String]
      else if(i == N) just("Stop")
      else sys.error("BOOM!")
    ) must_=== just("Stop")
  }

  "psumMap should be stack-safe and short-circuiting with IList" in {
    import Maybe.{empty, just}
    val N = 10000
    val xs = IList.fromList(List.range(1, 11000))
    xs.psumMap(i =>
      if(i < N) empty[String]
      else if(i == N) just("Stop")
      else sys.error("BOOM!")
    ) must_=== just("Stop")
  }

  "psumMap should be short-circuiting with NonEmptyList" in {
    import Maybe.{empty, just}
    val N = 10000
    val xs = NonEmptyList.nel(1, IList.fromList(List.range(2, 11000)))
    xs.psumMap(i =>
      if(i < N) empty[String]
      else if(i == N) just("Stop")
      else sys.error("BOOM!")
    ) must_=== just("Stop")
  }

  "non-empty folding" should {

    val gt1: (Int, Int)    => Int = (i, j) => i - j
    val gt2: (Int, => Int) => Int = (i, j) => i - j
    val strlen = (_ : String).length

    import syntax.foldable10._
    import syntax.std.list._

    "foldLeft1Opt" ! forAll {
      (xs: List[Int]) =>
        xs match {
          case Nil     => (xs foldLeft1Maybe gt1) must_== None
          case y :: ys => (xs foldLeft1Maybe gt1) must_== Some(ys.foldLeft(y)(gt1))
        }
    }

    "foldRight1Opt" ! forAll {
      (xs: List[Int]) =>
        xs match {
          case Nil => (xs foldRight1Maybe gt2) must_== None
          case _   => (xs foldRight1Maybe gt2) must_== Some(xs.init.foldRight(xs.last)(gt1))
        }
    }

    "foldl1Opt" ! forAll {
      (xs: List[Int]) =>
        xs match {
          case Nil     => (xs foldl1Maybe gt1.curried) must_== None
          case y :: ys => (xs foldl1Maybe gt1.curried) must_== Some(ys.foldLeft(y)(gt1))
        }
    }

    "foldr1Opt" ! forAll {
      (xs: List[Int]) =>
        xs match {
          case Nil => (xs foldr1Maybe gt2.curried) must_== None
          case _   => (xs foldr1Maybe gt2.curried) must_== Some(xs.init.foldRight(xs.last)(gt1))
        }
    }

    "foldMap1Opt" ! forAll {
      (xs: List[String]) =>
        xs.toNel match {
          case Empty()      => (xs foldMap1Maybe strlen) must_== None
          case Just(nel) => (xs foldMap1Maybe strlen) must_== Some(nel.foldMap1(strlen))
        }
    }

    "fold1Opt" ! forAll {
      (xs: List[Int]) => xs.fold1Maybe must_== xs.suml1Maybe
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

  "splitWith on sorted list produces at most 2 elements" ! forAll {
    (l: List[Int], p: Int => Boolean) =>
      L.splitWith(l.sortBy(p))(p).size mustBe_< 3
  }

  "splitWith consistent with partition" ! forAll {
    (l: List[Int], p: Int => Boolean) =>
      import scalaz.syntax.std.list._
      val (trueL, falseL) = l.partition(p)
      L.splitWith(l.sortBy(p))(p) must_=== List(falseL, trueL).flatMap(_.toNel)
  }

  "selectSplit: removes all non-satisfying elements" ! forAll {
    (l: List[Int]) =>
      L.selectSplit(l)(_ => false).size must_=== 0
  }

  "selectSplit: keeps all satisfying elements" ! forAll {
    (l: List[Int]) =>
      import scalaz.syntax.std.list._
      L.selectSplit(l)(_ => true) must_=== l.toNel.toList
  }

  "selectSplit: consistent with partition" ! forAll {
    (l: List[Int], p: Int => Boolean) =>
      L.selectSplit(l)(p).flatMap(_.toList) must_=== l.partition(p)._1
  }

  "selectSplit range consistent with filter" ! forAll {
    (i1: Byte, i2: Byte) =>
      val (s, e) = (Math.min(i1.toInt, i2.toInt), Math.max(i1.toInt, i2.toInt))
      val range = List.range(s, e)
      L.selectSplit(range)(_ % 2 != 0) must_=== range.filter(_ % 2 != 0).map(NonEmptyList(_))
  }

  /*
  "foldRight from foldMap" should {

    val fromFoldMap: Foldable[EphemeralStream] = new FromFoldMap[EphemeralStream] {
      override def foldMap[A, B](fa: EphemeralStream[A])(f: A => B)(implicit F: Monoid[B]): B = EphemeralStream.ephemeralStreamInstance.foldMap(fa)(f)
    }

    "foldRight is Lazy" in {
      val infiniteStream = EphemeralStream.iterate(0)(_ + 1)

      // This would failed with a StackOverflowError if foldRight was not lazy, which was the case with strict Endo:
      val stream: Stream[Int] = fromFoldMap.foldRight(infiniteStream, Stream.empty[Int]){ (i, is) => Stream.cons(i, is)}

      stream.take(100) must_=== infiniteStream.take(100).toStream
    }
  }
  */
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
