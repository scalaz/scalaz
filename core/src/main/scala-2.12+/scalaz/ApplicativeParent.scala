package scalaz

////
import Isomorphism.{ <~>, IsoFunctorTemplate }
import Tags.Parallel

////
trait ApplicativeParent[F[_]] { self: Applicative[F] =>
  ////

  /**
   * A lawful implementation of this that is isomorphic up to the methods
   * defined on Applicative allowing for optimised parallel implementations that
   * would otherwise violate laws of more specific typeclasses (e.g. Monad).
   */
  def par: Applicative[λ[α => F[α] @@ Parallel]] =
    Applicative.fromIso[λ[α => F[α] @@ Parallel], F](
      new IsoFunctorTemplate[λ[α => F[α] @@ Parallel], F] {
        override def from[A](ga: F[A]): F[A] @@ Parallel = Parallel(ga)
        override def to[A](fa: F[A] @@ Parallel): F[A] = Parallel.unwrap(fa)
      }
    )(self)

  ////
}
