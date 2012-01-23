package scalaz
package iteratee

import std.AllInstances._
import Iteratee._
import EnumeratorT._
import effect._

class IterateeTTest extends Spec {

  "head" in {
    (head[Unit, Int, Id] &= Stream(1, 2, 3)).runOrZero must be_===(Some(1))
  }

  "consume" in {
    (consume[Unit, Int, Id, List] &= Stream(1, 2, 3)).runOrZero must be_===(List(1, 2, 3))
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
      def semigroup[X, E, F[_]: Bind] = Semigroup[EnumeratorT[X, E, F]]
      def monoid[X, E, F[_]: Monad] = Monoid[EnumeratorT[X, E, F]]
      //def plus[X, E, F[_]: Bind, A] = Plus[({type λ[α]=EnumeratorT[X, E, F, α]})#λ]
      //def empty[X, E, F[_]: Monad, A] = PlusEmpty[({type λ[α]=EnumeratorT[X, E, F, α]})#λ]
    }

    object enumerator {
      //def monoid[X, E, A] = Monoid[Enumerator[X, E, A]]
    }
  }
}
