package scalaz.tc

trait PlusClass[F[_]] extends AltClass[F] {
  def empty[A]: F[A]
}
