package scalaz
package ct

trait ContravariantClass[F[_]] {
  def contramap[A, B](r: F[A])(f: B => A): F[B]
}

trait ContravariantFunctions {
  @inline final def contramap[F[_], A, B](fa: F[A])(f: B => A)(implicit F: Contravariant[F]): F[B] =
    F.contramap(fa)(f)
}
