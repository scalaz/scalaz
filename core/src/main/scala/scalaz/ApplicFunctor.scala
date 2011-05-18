package scalaz


trait ApplicFunctor[F[_]] {
  val applic: Applic[F]
  val functor: Functor[F]

  def fmap[A, B](f: A => B): F[A] => F[B] =
    functor.fmap(f)

  def apply[A, B](f: F[A => B]): F[A] => F[B] =
    applic.applic(f)

  def liftA2[A, B, C](f: A => B => C): F[A] => F[B] => F[C] =
    a => applic.applic(functor.fmap(f)(a))
}

object ApplicFunctor extends ApplicFunctors

trait ApplicFunctors {
  def applicFunctor[F[_]](implicit a: Applic[F], f: Functor[F]): ApplicFunctor[F] = new ApplicFunctor[F] {
    val applic = a
    val functor = f
  }

  implicit val OptionApplicFunctor: ApplicFunctor[Option] =
    applicFunctor

  implicit val ListApplicFunctor: ApplicFunctor[List] =
    applicFunctor

  implicit val StreamApplicFunctor: ApplicFunctor[Stream] =
    applicFunctor
}