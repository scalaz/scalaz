package scalaz
package iteratee

import std.AllInstances._
import Free.Trampoline
import Iteratee._
import effect._
import Id._

object IterateeTTest extends SpecLite {
  "head" in {
    (head[Int, Id] &= enumStream(Stream(1, 2, 3))).run must_===(Some(1))
  }

  "consume" in {
    (consume[Int, Id, List] &= enumStream(Stream(1, 2, 3))).run must_===(List(1, 2, 3))
  }

  "fold in constant stack space" in {
    val iter = fold[Int, Id, Int](0){ case (a,v) => a + v }.up[Trampoline]
    val enum = enumStream[Int, Trampoline](Stream.fill(10000)(1))
    (iter &= enum).run.run must_===(10000)
  }

  object instances {
    object iterateet {
      def monad[F[_]: Monad, E] = Monad[IterateeT[E, F, ?]]
      def liftIO[F[_]: MonadIO, E] = LiftIO[IterateeT[E, F, ?]]
      def monadIO[F[_]: MonadIO, E] = MonadIO[IterateeT[E, F, ?]]
    }

    object iteratee {
      def monad[E, F] = Monad[Iteratee[E, ?]]
    }
  }
}
