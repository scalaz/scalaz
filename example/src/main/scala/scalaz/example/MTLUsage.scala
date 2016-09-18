package scalaz
package example

import scalaz.syntax.monad._

object MTLUsage extends App {
  def app[F[_], E, R, S, W](implicit
    F0: Monad[F],
    F1: MonadPlus[F],
    F2: BindRec[F],
    F3: MonadError[F, E],
    F4: MonadReader[F, R],
    F5: MonadState[F, S],
    F6: MonadListen[F, W]
  ): F[Unit] = {
    val x = F4.ask

    /** monads work */
    val a = for {
      _ <- x
      _ <- x
    } yield 42

    ().pure[F]
  }
}
