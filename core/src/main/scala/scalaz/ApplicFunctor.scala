package scalaz


trait ApplicFunctor[F[_]] {
  val applic: Applic[F]
  val functor: Functor[F]

  import ApplicFunctor._

  def fmap[A, B](f: A => B): F[A] => F[B] =
    functor.fmap(f)

  def apply[A, B](f: F[A => B]): F[A] => F[B] =
    applic.applic(f)

  def liftA2[A, B, C](f: A => B => C): F[A] => F[B] => F[C] =
    a => applic.applic(functor.fmap(f)(a))

  def deriving[G[_]](implicit n: ^**^[G, F]): ApplicFunctor[G] = {
    implicit val a: Applic[G] = applic.deriving[G]
    implicit val f: Functor[G] = functor.deriving[G]
    applicFunctor[G]
  }

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

  implicit def Function1ApplicFunctor[T]: ApplicFunctor[({type λ[α] = Function1[T, α]})#λ] =
    applicFunctor[({type λ[α] = Function1[T, α]})#λ]
}