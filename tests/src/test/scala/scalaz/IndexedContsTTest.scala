package scalaz

import org.scalacheck.Arbitrary
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalazProperties._
import scalaz.std.anyVal._

object IndexedContsTTest extends SpecLite {

  type ContTMaybeBoolean[A] = ContT[Boolean, Maybe, A]

  private[this] implicit def contTEqual[A, F[_], B](implicit
    A: Arbitrary[A => F[B]],
    B: Equal[F[B]]
  ): Equal[ContT[B, F, A]] = {
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
    def functorRight[W[_]: Functor, R, M[_], O] = Functor[IndexedContsT[W, R, O, M, ?]]
    def functorLeft[W[_], O, M[_]: Functor, A] = Functor[IndexedContsT[W, ?, O, M, A]]
    def contravariant[W[_]: Functor, R, M[_]: Functor, A] = Contravariant[IndexedContsT[W, R, ?, M, A]]
    def bifunctor[W[_]: Functor, O, M[_]: Functor] = Bifunctor[IndexedContsT[W, ?, O, M, ?]]
    def bind[W[_]: Cobind, R, M[_]] = Bind[ContsT[W, R, M, ?]]
    def monad[W[_]: Comonad, R, M[_]] = Monad[ContsT[W, R, M, ?]]

    // checking absence of ambiguity
    def functor[W[_]: Comonad, R, M[_]] = Functor[ContsT[W, R, M, ?]]
    def functor[W[_]: Cobind, R, M[_]] = Functor[ContsT[W, R, M, ?]]
    def bind[W[_]: Comonad, R, M[_]] = Bind[ContsT[W, R, M, ?]]
  }
}
