package scalaz
package tc

import scala.language.experimental.macros

trait ContravariantClass[F[_]] extends InvariantFunctorClass[F] {

  def contramap[A, B](r: F[A])(f: B => A): F[B]

  override def imap[A, B](ma: F[A])(f: A => B)(g: B => A): F[B] = contramap(ma)(g)
}

trait ContravariantSyntax {
  implicit final class ToContravariantOps[F[_], A](fa: F[A]) {
    def contramap[B](f: B => A)(implicit ev: Contravariant[F]): F[B] = macro meta.Ops.i_1
  }
}
