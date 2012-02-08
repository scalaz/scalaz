package scalaz
package iteratee

import std.AllInstances._
import Iteratee._
import Enumeratee2T._
import effect._

class IterateeTTest extends Spec {
  "head" in {
    (head[Unit, Int, Id] &= enumStream(Stream(1, 2, 3))).runOrZero must be_===(Some(1))
  }

  "consume" in {
    (consume[Unit, Int, Id, List] &= enumStream(Stream(1, 2, 3))).runOrZero must be_===(List(1, 2, 3))
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
  }
}
