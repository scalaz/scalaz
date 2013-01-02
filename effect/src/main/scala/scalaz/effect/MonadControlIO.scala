package scalaz
package effect

////
/**
 *
 */
////
trait MonadControlIO[F[_]] extends LiftControlIO[F] with Monad[F] { self =>
  ////

  // derived functions

  ////
  val monadControlIOSyntax = new scalaz.syntax.effect.MonadControlIOSyntax[F] { def F = MonadControlIO.this }
}

object MonadControlIO {
  @inline def apply[F[_]](implicit F: MonadControlIO[F]): MonadControlIO[F] = F

  ////

  ////
}
