package scalaz
package typeclass

trait InvariantFunctor[F[_]] {
  def imap[A, B](ma: F[A])(f: A => B)(g: B => A): F[B]
}

object InvariantFunctor extends InvariantFunctorFunctions with InvariantFunctorSyntax {
  def apply[F[_]](implicit F: InvariantFunctor[F]): InvariantFunctor[F] = F
}
