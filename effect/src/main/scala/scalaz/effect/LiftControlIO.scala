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
  val liftControlIOSyntax: scalaz.syntax.effect.LiftControlIOSyntax[F] =
    new scalaz.syntax.effect.LiftControlIOSyntax[F] { def F = LiftControlIO.this }
}

object LiftControlIO {
  @inline def apply[F[_]](implicit F: LiftControlIO[F]): LiftControlIO[F] = F

  import Isomorphism._

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: LiftControlIO[G]): LiftControlIO[F] =
    new IsomorphismLiftControlIO[F, G] {
      override def G: LiftControlIO[G] = E
      override def iso: F <~> G = D
    }

  ////

  ////
}

trait IsomorphismLiftControlIO[F[_], G[_]] extends LiftControlIO[F] {
  implicit def G: LiftControlIO[G]
  ////
  import Isomorphism._
  def iso: F <~> G

  override def liftControlIO[A](f: IO.RunInBase[F, IO] => IO[A]): F[A] =
    iso.from(G.liftControlIO(f compose IO.hoistRunInBase(iso)))
  ////
}
