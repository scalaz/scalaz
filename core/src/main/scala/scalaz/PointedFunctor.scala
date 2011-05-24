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

  implicit def ConstPointedFunctor[A: Zero] =
    pointedFunctor[({type λ[α] = Const[A, α]})#λ]

  implicit val OptionPointedFunctor: PointedFunctor[Option] =
    pointedFunctor

  implicit val ListPointedFunctor: PointedFunctor[List] =
    pointedFunctor

  implicit val StreamPointedFunctor: PointedFunctor[Stream] =
    pointedFunctor
}
