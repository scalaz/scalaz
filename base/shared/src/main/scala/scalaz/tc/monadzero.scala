package scalaz.tc

trait MonadZeroClass[F[_]] extends MonadClass[F] with AlternativeClass[F] {}
