package scalaz
package ct

trait ContravariantClass[F[_]] extends InvariantFunctorClass[F] {

  def contramap[A, B](r: F[A])(f: B => A): F[B]

  override def imap[A, B](ma: F[A])(f: A => B)(g: B => A): F[B] = contramap(ma)(g)
}
