package scalaz
package ct

import scala.AnyVal

trait CategoryClass[=>:[_, _]] extends ComposeClass[=>:] {
  def id[A]: A =>: A
}

trait CategoryFunctions {
  @inline final def id[F[_, _], A](implicit F: Category[F]): F[A, A] =
    F.id[A]
  @inline final def id0[F[_, _]]: CategoryFunctions.id0[F] =
    new CategoryFunctions.id0[F]
  @inline final def id_∀[F[_, _]](implicit F: Category[F]): ∀[λ[α => F[α, α]]] =
    ∀.of[λ[α => F[α, α]]](F.id)
}

object CategoryFunctions {
  final class id0[F[_, _]] private[ct] (private val u: Unit = ()) extends AnyVal {
    @inline def apply[A](implicit F: Category[F]): F[A, A] =
      F.id[A]
  }
}
