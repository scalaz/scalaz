package scalaz

trait BindFunctor[F[_]] {
  val functor: Functor[F]
  val bind: Bind[F]

  import BindFunctor._

  def fmap[A, B](f: A => B): F[A] => F[B] =
    functor.fmap(f)

  def bind[A, B](f: A => F[B]): F[A] => F[B] =
    bind.bind(f)

  def deriving[G[_]](implicit n: ^**^[G, F]): BindFunctor[G] = {
    implicit val b: Bind[G] = bind.deriving[G]
    implicit val f: Functor[G] = functor.deriving[G]
    bindFunctor[G]
  }

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

  implicit def Tuple2BindFunctor[R: Semigroup] = {
    bindFunctor[({type λ[α] = (R, α)})#λ]
  }

  implicit def Tuple3BindFunctor[R: Semigroup, S: Semigroup] = {
    bindFunctor[({type λ[α] = (R, S, α)})#λ]
  }

  implicit def Tuple4BindFunctor[R: Semigroup, S: Semigroup, T: Semigroup] = {
    bindFunctor[({type λ[α] = (R, S, T, α)})#λ]
  }

  implicit def Tuple5BindFunctor[R: Semigroup, S: Semigroup, T: Semigroup, U: Semigroup] = {
    bindFunctor[({type λ[α] = (R, S, T, U, α)})#λ]
  }

  implicit def Tuple6BindFunctor[R: Semigroup, S: Semigroup, T: Semigroup, U: Semigroup, V: Semigroup] = {
    bindFunctor[({type λ[α] = (R, S, T, U, V, α)})#λ]
  }

  implicit def Tuple7BindFunctor[R: Semigroup, S: Semigroup, T: Semigroup, U: Semigroup, V: Semigroup, W: Semigroup] = {
    bindFunctor[({type λ[α] = (R, S, T, U, V, W, α)})#λ]
  }

}