package scalaz
package iteratee

import std.AllInstances._
import Iteratee._
import effect._

class IterateeTTest extends Spec {

  "head" in {
    (head[Unit, Int, Id].>>==(enumStream(Stream(1, 2, 3)))).runOrZero must be_===(Some(1))

    (head[Unit, Int, Id] >>== Stream(1, 2, 3)).runOrZero must be_===(Some(1))
  }

  object instances {
    object iterateet {
      def monad[F[_]: Monad, X, E] = Monad[({type λ[α] = IterateeT[X, E, F, α]})#λ]
      def liftIO[F[_]: MonadIO, X, E] = LiftIO[({type λ[α] = IterateeT[X, E, F, α]})#λ]
      def monadIO[F[_]: MonadIO, X, E] = MonadIO[({type λ[α] = IterateeT[X, E, F, α]})#λ]
    }

    object iteratee {
      def monad[X, E, F] = Monad[({type λ[α] = Iteratee[X, E, α]})#λ]
    }

    object enumeratorT {
      def semigroup[X, E, F[_]: Bind, A] = Semigroup[EnumeratorT[X, E, F, A]]
      def monoid[X, E, F[_]: Monad, A] = Monoid[EnumeratorT[X, E, F, A]]
    }

    object enumerator {
      def monoid[X, E, A] = Monoid[Enumerator[X, E, A]]
    }
  }
}