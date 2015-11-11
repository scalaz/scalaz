package scalaz
package effect

////
/**
 *
 */
////
trait LiftControlIO[F[_]]  { self =>
  ////
  import IO._

  def liftControlIO[A](f: RunInBase[F, IO] => IO[A]): F[A]

  // derived functions

  ////
  @transient lazy val liftControlIOSyntax = new scalaz.syntax.effect.LiftControlIOSyntax[F] { def F = LiftControlIO.this }
}

object LiftControlIO {
  @inline def apply[F[_]](implicit F: LiftControlIO[F]): LiftControlIO[F] = F

  ////

  ////
}
