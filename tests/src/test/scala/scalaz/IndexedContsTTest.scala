package scalaz

import org.scalacheck.Arbitrary
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

  object instances {
    def functorRight[W[_]: Functor, M[_], R, O] = Functor[IndexedContsT[W, M, R, O, ?]]
    def functorLeft[W[_], M[_]: Functor, O, A] = Functor[IndexedContsT[W, M, ?, O, A]]
    def contravariant[W[_]: Functor, M[_]: Functor, R, A] = Contravariant[IndexedContsT[W, M, R, ?, A]]
    def bifunctor[W[_]: Functor, M[_]: Functor, O] = Bifunctor[IndexedContsT[W, M, ?, O, ?]]
    def bind[W[_]: Cobind, M[_], R] = Bind[ContsT[W, M, R, ?]]
    def monad[W[_]: Comonad, M[_], R] = Monad[ContsT[W, M, R, ?]]

    // checking absence of ambiguity
    def functor[W[_]: Comonad, M[_], R] = Functor[ContsT[W, M, R, ?]]
    def functor[W[_]: Cobind, M[_], R] = Functor[ContsT[W, M, R, ?]]
    def bind[W[_]: Comonad, M[_], R] = Bind[ContsT[W, M, R, ?]]
  }
}
