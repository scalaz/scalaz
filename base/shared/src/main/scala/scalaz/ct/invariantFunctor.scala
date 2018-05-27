package scalaz
package ct

import scala.language.experimental.macros

trait InvariantFunctorClass[F[_]] {
  def imap[A, B](ma: F[A])(f: A => B)(g: B => A): F[B]
}

trait InvariantFunctorFunctions {
  def imap[F[_], A, B](fa: F[A])(f: A => B)(g: B => A)(implicit F: InvariantFunctor[F]): F[B] = F.imap(fa)(f)(g)
}

trait InvariantFunctorSyntax {
  implicit final class ToInvariantFunctorOps[F[_], A](self: F[A]) {
    def imap[B](f: A => B)(g: B => A)(implicit ev: InvariantFunctor[F]): F[B] = macro meta.Ops.i_1_1
  }
}
