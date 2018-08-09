package scalaz
package laws

import scala.{ inline }
import scala.Predef.identity

import tc._

object FunctorLaws {

  // the "composition" laws (e.g., `fmap f . fmap g = fmap (f . g)`)
  // for all kinds of functors are guaranteed by
  // parametricity plus their "identity" laws.
  object Functor {
    // Because a `Functor` has no means of inspecting the
    // `A` it's parameterized with at runtime, one implication
    // of this law is that `fmap` does not modify its functorial context.
    @inline
    def identityToIdentity[F[_], A, T](in: F[A])(assert: (F[A], F[A]) => T)(implicit F: Functor[F]): T =
      assert(in, F.map(in)(identity))
  }

  object Contravariant {
    // The same applies to `Contravariant`; in fact, to every
    // kind of functor from and products of `Scal` and `Scal^op` to `Scal`.
    @inline
    def identityToIdentity[F[_], A, T](in: F[A])(assert: (F[A], F[A]) => T)(implicit F: Contravariant[F]): T =
      assert(in, F.contramap(in)(identity))
  }

  object InvariantFunctor {
    @inline
    def identityToIdentity[F[_], A, T](in: F[A])(assert: (F[A], F[A]) => T)(implicit F: InvariantFunctor[F]): T =
      assert(in, F.imap(in)(identity)(identity))
  }

  object Phantom {
    // `pmap`, since it can't modify any `A` values,
    // and because of the identity law it can't modify the `F[_]`
    // context, must do nothing at all but return its argument.
    @inline
    def identityToIdentity[F[_], A, T](in: F[A])(assert: (F[A], F[A]) => T)(implicit F: Phantom[F]): T =
      assert(in, F.pmap(in))
  }

  object Bifunctor {
    @inline
    def identityToIdentity[F[_, _], A, B, T](
      in: F[A, B]
    )(assert: (F[A, B], F[A, B]) => T)(implicit F: Bifunctor[F]): T =
      assert(in, F.bimap(in)(identity, identity))
  }

  object Profunctor {
    @inline
    def identityToIdentity[P[_, _], A, B, T](
      in: P[A, B]
    )(assert: (P[A, B], P[A, B]) => T)(implicit P: Profunctor[P]): T =
      assert(in, P.dimap(in)((a: A) => a)((b: B) => b))
  }
}
