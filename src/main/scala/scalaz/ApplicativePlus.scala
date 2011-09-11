package scalaz

trait ApplicativePlusLike[F[_]] extends ApplicativeLike[F] with PlusLike[F]
trait ApplicativePlus[F[_]] extends ApplicativePlusLike[F]
trait ApplicativePlusInstance[F[_]] extends ApplicativePlus[F] with PlusInstance[F] with ApplicativeInstance[F]
