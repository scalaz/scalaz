package scalaz

import syntax.all._
import Inject._

object InjectTest extends SpecLite {
  import std.AllInstances._
  import Test1Algebra._, Test2Algebra._, Test3Algebra._

  sealed trait Test1Algebra[A]
  case class Test1[A](keys: Seq[String], h: Int => A) extends Test1Algebra[A]

  sealed trait Test1AlgebraInstances {
    implicit def test1AlgebraAFunctor: Functor[Test1Algebra] =
      new Functor[Test1Algebra] {
        def map[A, B](a: Test1Algebra[A])(f: A => B): Test1Algebra[B] = a match {
          case Test1(k, h) => Test1(k, x => f(h(x)))
        }
      }
  }

  sealed trait Test1AlgebraFunctions {
    def test1[F[_]](keys: Seq[String])(implicit F: Functor[F], I: Test1Algebra :<: F): Free[F, Int] =
      inject[F, Test1Algebra, Int](Test1(keys, Free.pure(_)))
  }

  object Test1Algebra extends Test1AlgebraInstances with Test1AlgebraFunctions

  sealed trait Test2Algebra[A]
  case class Test2[A](keys: Seq[String], h: Int => A) extends Test2Algebra[A]

  sealed trait Test2AlgebraInstances {
    implicit def test2AlgebraAFunctor: Functor[Test2Algebra] =
      new Functor[Test2Algebra] {
        def map[A, B](a: Test2Algebra[A])(f: A => B): Test2Algebra[B] = a match {
          case Test2(k, h) => Test2(k, x => f(h(x)))
        }
      }
  }

  sealed trait Test2AlgebraFunctions {
    def test2[F[_]](keys: Seq[String])(implicit F: Functor[F], I: Test2Algebra :<: F): Free[F, Int] =
      inject[F, Test2Algebra, Int](Test2(keys, Free.pure(_)))
  }

  object Test2Algebra extends Test2AlgebraInstances with Test2AlgebraFunctions

  sealed trait Test3Algebra[A]
  case class Test3[A](keys: Seq[String], h: Int => A) extends Test3Algebra[A]

  sealed trait Test3AlgebraInstances {
    implicit def test3AlgebraAFunctor: Functor[Test3Algebra] =
      new Functor[Test3Algebra] {
        def map[A, B](a: Test3Algebra[A])(f: A => B): Test3Algebra[B] = a match {
          case Test3(k, h) => Test3(k, x => f(h(x)))
        }
      }
  }

  sealed trait Test3AlgebraFunctions {
    def test3[F[_]](keys: Seq[String])(implicit F: Functor[F], I: Test3Algebra :<: F): Free[F, Int] =
      inject[F, Test3Algebra, Int](Test3(keys, Free.pure(_)))
  }

  object Test3Algebra extends Test3AlgebraInstances with Test3AlgebraFunctions

  type C0[A] = Coproduct[Test1Algebra, Test2Algebra, A]
  type C1[A] = Coproduct[Test3Algebra, C0, A]
  type T[A] = C1[A]

  "inj" in {
    def run[A](algebra: Free[T, A]): A =
      algebra.resume.fold({
        case Coproduct(-\/(Test3(k, h))) => run(h(k.length))
        case Coproduct(\/-(Coproduct(-\/(Test1(k, h))))) => run(h(k.length))
        case Coproduct(\/-(Coproduct(\/-(Test2(k, h))))) => run(h(k.length))
      }, a => a)

    val res =
      for {
        a <- test1[T](Seq("a1", "a2", "a3"))
        b <- test2[T](Seq("b1"))
        c <- test3[T](Seq("c1", "c2"))
      } yield (a, b, c)

    run(res) must_===((3, 1, 2))
  }

  "prj" in {
    def distr[F[_], A](t: Free[F, A])
      (implicit
        F: Functor[F],
        I0: Test1Algebra :<: F,
        I1: Test2Algebra :<: F,
        I2: Test3Algebra :<: F): Option[Free[F, A]] =
          for {
            Test1(x, h) <- match_[F, Test1Algebra, A](t)
            Test2(y, k) <- match_[F, Test2Algebra, A](h(x.length))
            Test3(z, l) <- match_[F, Test3Algebra, A](k((x ++ y).length))
          } yield l((x ++ y ++ z).length)

    val res =
      distr[T, Int]((test1[T](Seq("a")) >> test2[T](Seq("b")): Free[T, Int]) >> test3[T](Seq("c")))

    (res == Some(Free.pure[T, Int](3))) must_===(true)
  }

  "apply in left" in {
    val fa = Test1(Seq("a"), Free.pure[Test1Algebra, Int](_))
    (Inject[Test1Algebra, C0].inj(fa) == Coproduct(-\/(fa))) must_===(true)
  }

  "apply in right" in {
    val fa = Test2(Seq("a"), Free.pure[Test2Algebra, Int](_))
    (Inject[Test2Algebra, C0].inj(fa) == Coproduct(\/-(fa))) must_===(true)
  }

  "unapply from left" in {
    val fa = Test1(Seq("a"), Free.pure[Test1Algebra, Int](_))
    val T1A = Inject[Test1Algebra, C0]

    T1A(fa) match {
      case T1A(check) => check must_== fa
      case _          => fail("Wrong coproduct")
    }
  }

  "unapply from right" in {
    val fa = Test2(Seq("a"), Free.pure[Test2Algebra, Int](_))
    val T2A = Inject[Test2Algebra, C0]

    T2A(fa) match {
      case T2A(check) => check must_== fa
      case _          => fail("Wrong coproduct")
    }
  }
}
