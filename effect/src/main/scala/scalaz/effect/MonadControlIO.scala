package scalaz
package effect

// TODO instances, maybe remove and pass LiftControlIO and Monad separately.
trait MonadControlIO[F[_]] extends LiftControlIO[F] with Monad[F] {
}

object MonadControlIO extends MonadControlIOs

trait MonadControlIOs {
}