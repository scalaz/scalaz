package scalaz

import org.scalacheck.{Arbitrary, Gen}
import Isomorphism._
import std.AllInstances._
import scalaz.scalacheck.ScalazProperties.monad
import scalaz.scalacheck.ScalaCheckBinding._
import FreeTest._
import Id.Id

object TrampolineTTest extends SpecLite {

  implicit def trampolineTArb[M[_], A](implicit
    M: Arbitrary ~> ({type λ[α] = Arbitrary[M[α]]})#λ,
    N: Functor[M],
    A: Arbitrary[A]
  ): Arbitrary[TrampolineT[M, A]] =
    Arbitrary(Gen.oneOf(
      Functor[Arbitrary].map(M(A))(TrampolineT.done).arbitrary,
      Functor[Arbitrary].map(M(trampolineTArb[M, A]))(x =>
        TrampolineT.more(N.map(x)(() => _))
      ).arbitrary
    ))

  type TrampolineTOpt[A]  = TrampolineT[Option, A]
  type TrampolineTList[A] = TrampolineT[List, A]
  type TrampolineTOneAndOpt[A] = TrampolineT[OneAndOpt, A]

  implicit val optArb = new FreeTest.Template[Option, Arbitrary] {
    def lift[A: Arbitrary] = implicitly
  }

  implicit val optEq = new FreeTest.Template[Option, Equal] {
    def lift[A: Equal] = implicitly
  }

  checkAll(monad.laws[TrampolineTOpt])
  checkAll(monad.laws[TrampolineTList])
  checkAll(monad.laws[TrampolineTOneAndOpt])

  private val fibResult = 317811
  private val fibParam = 28

  "fibonacci" ! {
    def fib(n: Int): TrampolineT[Id, Int] =
      if (n < 2) TrampolineT.done[Id, Int](n)
      else TrampolineT.trampolineTMonad[Id].apply2(
        TrampolineT.suspend(fib(n - 1)),
        TrampolineT.suspend(fib(n - 2))
      )(_ + _)

    fib(fibParam).run must_=== fibResult

    val nt = new (Id ~> Option){def apply[A](a: A) = Option(a)}
    fib(fibParam).trans(nt).run must_=== Option(fibResult)
  }

  "from" ! {
    def fib(n: Int): Free.Trampoline[Int] =
      if (n < 2) Trampoline.done(n)
      else Apply[Free.Trampoline].apply2(
        Trampoline.suspend(fib(n - 1)),
        Trampoline.suspend(fib(n - 2))
      )(_ + _)

    TrampolineT.from[Id, Int](fib(fibParam)).run must_=== fibResult
    fib(fibParam).run must_=== fibResult
  }

  "kleisli" ! {
    val a = 100000
    val b = 10
    val k = Kleisli[Option, IList[Int], IList[Int]](_.tailOption)
    val endo = k.liftMK[TrampolineT].endo
    val m = Endomorphic.kleisliEndoInstance[({type λ[α] = TrampolineT[Option, α]})#λ, IList[Int]]
    m.multiply(endo, a).run.run(IList.fill(a + b)(0)).run.map(_.length) must_=== Option(b)
  }

  "no StackOverflowError monadic trampoline functions" should {

    def cons[A]: A => IList[A] = (_: A) :: IList.empty[A]

    val constTrue: Int => Id[Boolean]  = (_: Int) => true
    val constFalse: Int => Id[Boolean] = (_: Int) => false

    val constTrueList  = constTrue andThen cons
    val constFalseList = constFalse andThen cons

    "List" should {
      import syntax.std.list._
      val a = (1 to 100000).toList

      "takeWhileMTrampoline" ! {
        a.takeWhileMTrampoline(constTrue) must_=== a
        a.takeWhileMTrampoline(constTrueList) must_=== cons(a)
      }
      "takeUntilMTrampoline" ! {
        a.takeUntilMTrampoline(constFalse) must_=== a
        a.takeUntilMTrampoline(constFalseList) must_=== cons(a)
      }
      "filterMTrampoline" ! {
        a.filterMTrampoline(constTrue) must_=== a
        a.filterMTrampoline(constTrueList) must_=== cons(a)
      }
      "findMTrampoline" ! {
        a.findMTrampoline(constFalse) must_=== None
        a.findMTrampoline(constFalseList) must_=== cons(None)
      }
      "partitionMTrampoline" ! {
        val f = (_: Int) % 3 == 0
        a.partitionMTrampoline[Id](f) must_=== a.partition(f)
        a.partitionMTrampoline(f andThen cons) must_=== cons(a.partition(f))
      }
      "spanMTrampoline" ! {
        a.spanMTrampoline(constTrue) must_=== a.span(constTrue)
        a.spanMTrampoline(constTrueList) must_=== cons(a.span(constTrue))
      }
      "breakMTrampoline" ! {
        a.breakMTrampoline(constFalse) must_=== a.span(constTrue)
        a.breakMTrampoline(constFalseList) must_=== cons(a.span(constTrue))
      }
      "groupWhenMTrampoline" ! {
        import syntax.functor._
        val f = (_: Int, _: Int) => true
        a.groupWhenMTrampoline[Id](f) must_=== a.groupWhen(f)
        a.groupWhenMTrampoline(f map cons) must_=== cons(a.groupWhen(f))
      }
    }

    "Vector" should {
      import syntax.std.vector._
      val a: Vector[Int] = Foldable[List].to[Int, Vector](1 to 100000 toList)

      "takeWhileMTrampoline" ! {
        a.takeWhileMTrampoline(constTrue) must_=== a
        a.takeWhileMTrampoline(constTrueList) must_=== cons(a)
      }
      "takeUntilMTrampoline" ! {
        a.takeUntilMTrampoline(constFalse) must_=== a
        a.takeUntilMTrampoline(constFalseList) must_=== cons(a)
      }
      "filterMTrampoline" ! {
        a.filterMTrampoline(constTrue) must_=== a
        a.filterMTrampoline(constTrueList) must_=== cons(a)
      }
      "findMTrampoline" ! {
        a.findMTrampoline(constFalse) must_=== None
        a.findMTrampoline(constFalseList) must_=== cons(None)
      }
      "partitionMTrampoline" ! {
        val f = (_: Int) % 4 != 0
        a.partitionMTrampoline[Id](f) must_=== a.partition(f)
        a.partitionMTrampoline(f andThen cons) must_=== cons(a.partition(f))
      }
      "spanMTrampoline" ! {
        a.spanMTrampoline(constTrue) must_=== a.span(constTrue)
        a.spanMTrampoline(constTrueList) must_=== cons(a.span(constTrue))
      }
      "breakMTrampoline" ! {
        a.breakMTrampoline(constFalse) must_=== a.span(constTrue)
        a.breakMTrampoline(constFalseList) must_=== cons(a.span(constTrue))
      }
      "groupWhenMTrampoline" ! {
        import syntax.functor._
        val f = (_: Int, _: Int) => true
        a.groupWhenMTrampoline[Id](f) must_=== a.groupWhen(f)
        a.groupWhenMTrampoline(f map cons) must_=== cons(a.groupWhen(f))
      }
    }

    "Foldable" should {
      val a = 1 to 100000
      import syntax.foldable._

      "foldLeftMTrampoline" ! {
        val f1 = (x: Int, y: Int) => Option(x + y)
        a.toList.foldLeftMTrampoline(0)(f1) must_=== Some(a.sum)
        a.toStream.foldLeftMTrampoline(0)(f1) must_=== Some(a.sum)
        EphemeralStream(a: _*).foldLeftMTrampoline(0)(f1) must_=== Some(a.sum)

        a.toList.foldLeftMTrampoline[Id, Vector[Int]](Vector[Int]())(_ :+ _) must_=== a.toList.foldLeft(Vector[Int]())(_ :+ _)
      }

      "foldRightMTrampoline" ! {
        val f1: (Int, => Int) => Option[Int] = (x, y) => Option(x + y)
        a.toList.foldRightMTrampoline(0)(f1) must_=== Some(a.sum)
        a.toStream.foldRightMTrampoline(0)(f1) must_=== Some(a.sum)
        EphemeralStream(a: _*).foldRightMTrampoline(0)(f1) must_=== Some(a.sum)

        a.toList.foldRightMTrampoline[Id, List[Int]](List[Int]())(_ :: _) must_=== Foldable[List].foldRight(a.toList, List[Int]())(_ :: _)
      }
    }
  }
}

