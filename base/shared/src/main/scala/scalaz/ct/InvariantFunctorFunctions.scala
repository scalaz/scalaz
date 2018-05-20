package scalaz
package ct

trait InvariantFunctorFunctions {
  def imap[F[_], A, B](fa: F[A])(f: A => B)(g: B => A)(implicit F: InvariantFunctor[F]): F[B] = F.imap(fa)(f)(g)
}
