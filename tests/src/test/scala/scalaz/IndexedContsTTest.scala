package scalaz

import org.scalacheck.{Arbitrary, Cogen}

import scalaz.Free.Trampoline
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalazProperties._
import scalaz.std.anyVal._

object IndexedContsTTest extends SpecLite {

  type ContTMaybeBoolean[A] = ContT[Maybe, Boolean, A]

  private[this] implicit def contTEqual[F[_], A, B](implicit
    A: Arbitrary[A => F[B]],
    B: Equal[F[B]]
  ): Equal[ContT[F, B, A]] = {
    val functions = Stream.continually(
      A.arbitrary.sample
    ).flatten.take(5).toList

    Equal.equal{ (a, b) =>
      functions.forall{ f =>
        B.equal(a(f), b(f))
      }
    }
  }

  checkAll(monadPlus.laws[ContTMaybeBoolean])

  private[this] implicit def trampolineArbitrary[A](implicit arbitraryA: Arbitrary[A]): Arbitrary[Trampoline[A]] = {
    Arbitrary(arbitraryA.arbitrary.map(Trampoline.done(_)))
  }

  private[this] implicit def cogenTrampoline[A](implicit cogenA: Cogen[A]): Cogen[Trampoline[A]] = {
    cogenA.contramap(_.run)
  }

  private[this] implicit def trampolineEqual[A](implicit equalA: Equal[A]): Equal[Trampoline[A]] = {
    equalA.contramap(_.run)
  }

  type ContTTrampolineBoolean[A] = ContT[Trampoline, Boolean, A]

  checkAll(monad.laws[ContTTrampolineBoolean])

  "cont with trampoline" should {
    "flatMap does not blow stack" in {
      val N = 100000

      type ContTTrampolineInt[A] = ContT[Trampoline, Int, A]

      (0 until N)
        .foldLeft(Monad[ContTTrampolineInt].point(())) { (a, _) =>
          Monad[ContTTrampolineInt].bind(a)(_ => Monad[ContTTrampolineInt].point(()))
        }
        .run(_ => Trampoline.done(N)).run must_=== N // No stack overflow

      def loop(i: Int): ContTTrampolineInt[Unit] =
        if(i > 0) Monad[ContTTrampolineInt].bind(Monad[ContTTrampolineInt].point(()))(_ => loop(i-1))
        else Monad[ContTTrampolineInt].point(())

      loop(N).run(_ => Trampoline.done(N)).run must_=== N // No stack overflow
    }
  }

  object instances {
    def functorRight[W[_]: Functor, M[_], R, O] =
      Functor[IndexedContsT[W, M, R, O, ?]]
    def functorLeft[W[_], M[_]: Functor, O, A] =
      Functor[IndexedContsT[W, M, ?, O, A]]
    def contravariant[W[_]: Functor, M[_]: Functor, R, A] =
      Contravariant[IndexedContsT[W, M, R, ?, A]]
    def bifunctor[W[_]: Functor, M[_]: Functor, O] =
      Bifunctor[IndexedContsT[W, M, ?, O, ?]]
    def bind[W[_]: Cobind, M[_], R] = Bind[ContsT[W, M, R, ?]]
    def monad[W[_]: Comonad, M[_], R] = Monad[ContsT[W, M, R, ?]]

    // checking absence of ambiguity
    def functor[W[_]: Comonad, M[_], R] = Functor[ContsT[W, M, R, ?]]
    def functor[W[_]: Cobind, M[_], R] = Functor[ContsT[W, M, R, ?]]
    def bind[W[_]: Comonad, M[_], R] = Bind[ContsT[W, M, R, ?]]

    // stack safe type classes
    def stackSafeFunctor[W[_]: Functor, S[_], R] =
      Functor[ContsT[W, Free[S, ?], R, ?]]
    def stackSafeApplicative[W[_]: Comonad, S[_], R] =
      Applicative[ContsT[W, Free[S, ?], R, ?]]
    def stackSafeBind[W[_]: Cobind, S[_], R] =
      Bind[ContsT[W, Free[S, ?], R, ?]]
    def stackSafeBindRec[W[_]: Cobind, S[_], R] =
      BindRec[ContsT[W, Free[S, ?], R, ?]]
    def stackSafeBindRec[W[_]: Comonad, S[_], R] =
      BindRec[ContsT[W, Free[S, ?], R, ?]]
    def stackSafeMonad[W[_]: Comonad, S[_], R] =
      Monad[ContsT[W, Free[S, ?], R, ?]]
  }
}
