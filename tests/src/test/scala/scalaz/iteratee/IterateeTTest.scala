package scalaz
package iteratee

import std.AllInstances._
import Iteratee._
import effect._
import Id._

class IterateeTTest extends Spec {
  "head" in {
    (head[Int, Id] &= enumStream(Stream(1, 2, 3))).run must be_===(Some(1))
  }

  "consume" in {
    (consume[Int, Id, List] &= enumStream(Stream(1, 2, 3))).run must be_===(List(1, 2, 3))
  }

  "fold in constant stack space" in {
    skipped("TODO")
    (fold[Int, Id, Int](0){ case (a,v) => a + v } &= enumStream[Int, Id](Stream.fill(10000)(1))).run must be_===(10000)
  }

  object instances {
    object iterateet {
      def monad[F[_]: Monad, E] = Monad[({type λ[α] = IterateeT[E, F, α]})#λ]
      def liftIO[F[_]: MonadIO, E] = LiftIO[({type λ[α] = IterateeT[E, F, α]})#λ]
      def monadIO[F[_]: MonadIO, E] = MonadIO[({type λ[α] = IterateeT[E, F, α]})#λ]
    }

    object iteratee {
      def monad[E, F] = Monad[({type λ[α] = Iteratee[E, α]})#λ]
    }
  }
}
