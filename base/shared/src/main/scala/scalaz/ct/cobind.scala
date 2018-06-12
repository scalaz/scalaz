package scalaz
package ct

import scala.language.experimental.macros

trait CobindClass[F[_]] extends FunctorClass[F] {

  def cobind[A, B](fa: F[A])(f: F[A] => B): F[B]

  def cojoin[A](fa: F[A]): F[F[A]]
}

object CobindClass {

  trait DeriveCojoin[F[_]] extends CobindClass[F] with Alt[DeriveCojoin[F]] {
    final override def cojoin[A](fa: F[A]): F[F[A]] = cobind(fa)(a => a)
  }

  trait DeriveCobind[F[_]] extends CobindClass[F] with Alt[DeriveCobind[F]] {
    final override def cobind[A, B](fa: F[A])(f: F[A] => B): F[B] = map(cojoin(fa))(f)
  }

  trait Alt[D <: Alt[D]]

}

trait CobindSyntax {
  implicit final class ToCobindOps[F[_], A](fa: F[A]) {
    def cobind[B](f: F[A] => B)(implicit ev: Cobind[F]): F[B] = macro meta.Ops.i_1
  }
}
