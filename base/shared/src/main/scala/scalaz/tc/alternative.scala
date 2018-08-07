package scalaz.tc

trait AlternativeClass[F[_]] extends ApplicativeClass[F] with PlusClass[F] {}
