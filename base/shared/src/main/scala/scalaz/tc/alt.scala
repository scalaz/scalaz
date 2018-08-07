package scalaz.tc

trait AltClass[F[_]] extends FunctorClass[F] {
  def alt[A](fa1: => F[A], fa2: => F[A]): F[A]
}
