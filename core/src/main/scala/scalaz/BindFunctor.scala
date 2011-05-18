package scalaz

trait BindFunctor[F[_]] {
  val functor: Functor[F]
  val bind: Bind[F]

  def fmap[A, B](f: A => B): F[A] => F[B] =
    functor.fmap(f)

  def bind[A, B](f: A => F[B]): F[A] => F[B] =
    bind.bind(f)
}

object BindFunctor extends BindFunctors

trait BindFunctors {
  def bindFunctor[F[_]](implicit f: Functor[F], b: Bind[F]): BindFunctor[F] = new BindFunctor[F] {
    val functor = f
    val bind = b
  }

  implicit val OptionBindFunctor: BindFunctor[Option] =
    bindFunctor

  implicit val ListBindFunctor: BindFunctor[List] =
    bindFunctor

  implicit val StreamBindFunctor: BindFunctor[Stream] =
    bindFunctor
}