package scalaz

trait PointedFunctor[F[_]] {
  val functor: Functor[F]
  val pointed: Pointed[F]

  import PointedFunctor._

  def compose[G[_]](gtr: PointedFunctor[G]): PointedFunctor[({type λ[α] = F[G[α]]})#λ] =
    pointedFunctor[({type λ[α] = F[G[α]]})#λ](
      new Functor[({type λ[α] = F[G[α]]})#λ] {
        def fmap[A, B](f: A => B) =
          functor.fmap(gtr.fmap(f))
      }
      , new Pointed[({type λ[α] = F[G[α]]})#λ] {
        def point[A](a: => A) =
          pointed.point(gtr.point(a))
      }
    )

  def **[G[_] : PointedFunctor]: PointedFunctor[({type λ[α] = (F[α], G[α])})#λ] = {
    implicit val f = functor ** implicitly[PointedFunctor[G]].functor
    implicit val p = pointed ** implicitly[PointedFunctor[G]].pointed
    pointedFunctor[({type λ[α] = (F[α], G[α])})#λ]
  }

  def point[A](a: => A): F[A] =
    pointed.point(a)

  def fmap[A, B](f: A => B): F[A] => F[B] =
    functor.fmap(f)

  def deriving[G[_]](implicit n: ^**^[G, F]): PointedFunctor[G] = {
    implicit val f: Functor[G] = functor.deriving[G]
    implicit val p: Pointed[G] = pointed.deriving[G]
    pointedFunctor[G]
  }

}

object PointedFunctor extends PointedFunctors

trait PointedFunctors {
  def pointedFunctor[F[_]](implicit f: Functor[F], p: Pointed[F]): PointedFunctor[F] = new PointedFunctor[F] {
    val functor = f
    val pointed = p
  }

  implicit val OptionPointedFunctor: PointedFunctor[Option] =
    pointedFunctor

  implicit val ListPointedFunctor: PointedFunctor[List] =
    pointedFunctor

  implicit val StreamPointedFunctor: PointedFunctor[Stream] =
    pointedFunctor

  implicit def Tuple2PointedFunctor[R: Zero] =
    pointedFunctor[({type λ[α] = (R, α)})#λ]

  implicit def Tuple3PointedFunctor[R: Zero, S: Zero] =
    pointedFunctor[({type λ[α] = (R, S, α)})#λ]

  implicit def Tuple4PointedFunctor[R: Zero, S: Zero, T: Zero] =
    pointedFunctor[({type λ[α] = (R, S, T, α)})#λ]

  implicit def Tuple5PointedFunctor[R: Zero, S: Zero, T: Zero, U: Zero] =
    pointedFunctor[({type λ[α] = (R, S, T, U, α)})#λ]

  implicit def Tuple6PointedFunctor[R: Zero, S: Zero, T: Zero, U: Zero, V: Zero] =
    pointedFunctor[({type λ[α] = (R, S, T, U, V, α)})#λ]

  implicit def Tuple7PointedFunctor[R: Zero, S: Zero, T: Zero, U: Zero, V: Zero, W: Zero] =
    pointedFunctor[({type λ[α] = (R, S, T, U, V, W, α)})#λ]

  implicit def Function0PointedFunctor: PointedFunctor[Function0] =
    pointedFunctor[Function0]

  implicit def Function1PointedFunctor[R]: PointedFunctor[({type λ[α]=(R) => α})#λ] =
    pointedFunctor[({type λ[α]=(R) => α})#λ]

  implicit def Function2PointedFunctor[R, S]: PointedFunctor[({type λ[α]=(R, S) => α})#λ] =
    pointedFunctor[({type λ[α]=(R, S) => α})#λ]

  implicit def Function3PointedFunctor[R, S, T]: PointedFunctor[({type λ[α]=(R, S, T) => α})#λ] =
    pointedFunctor[({type λ[α]=(R, S, T) => α})#λ]

  implicit def Function4PointedFunctor[R, S, T, U]: PointedFunctor[({type λ[α]=(R, S, T, U) => α})#λ] =
    pointedFunctor[({type λ[α]=(R, S, T, U) => α})#λ]

  implicit def Function5PointedFunctor[R, S, T, U, V]: PointedFunctor[({type λ[α]=(R, S, T, U, V) => α})#λ] =
    pointedFunctor[({type λ[α]=(R, S, T, U, V) => α})#λ]

  implicit def Function6PointedFunctor[R, S, T, U, V, W]: PointedFunctor[({type λ[α]=(R, S, T, U, V, W) => α})#λ] =
    pointedFunctor[({type λ[α]=(R, S, T, U, V, W) => α})#λ]
    
}
