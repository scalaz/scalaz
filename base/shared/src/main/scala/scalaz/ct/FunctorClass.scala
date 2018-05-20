package scalaz
package ct

trait FunctorClass[F[_]] extends InvariantFunctorClass[F] {

  def map[A, B](ma: F[A])(f: A => B): F[B]

  final override def imap[A, B](ma: F[A])(f: A => B)(g: B => A): F[B] = map(ma)(f)
}
