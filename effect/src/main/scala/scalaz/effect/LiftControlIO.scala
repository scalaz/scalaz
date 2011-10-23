package scalaz
package effect

trait LiftControlIO[F[_]] {

  import effect.IO, IO._

  def liftControlIO[A](f: RunInBase[F, IO] => IO[A]): F[A]
}

object LiftControlIO extends LiftControlIOs

trait LiftControlIOs
