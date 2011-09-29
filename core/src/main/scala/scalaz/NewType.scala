package scalaz

object NewType {
  @inline def apply[@specialized A, T](a: A): A ## T = a.asInstanceOf[A ## T]

  def subst[A, F[_], T](fa: F[A])(implicit F: Functor[F]): F[A ## T] = fa.asInstanceOf[F[A ## T]]

  def unsubst[A, F[_], T](fa: F[A ## T])(implicit F: Functor[F]): F[A] = fa.asInstanceOf[F[A]]

  def contraSubst[A, F[_], T](fa: F[A ## T])(implicit F: Contravariant[F]): F[A] = fa.asInstanceOf[F[A]]

  def contraUnsubst[A, F[_], T](fa: F[A])(implicit F: Functor[F]): F[A ## T] = fa.asInstanceOf[F[A ## T]]
}