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

  implicit def EitherLeftPointedFunctor[X] =
    pointedFunctor[({type λ[α]=Either.LeftProjection[α, X]})#λ]

  implicit def EitherRightPointedFunctor[X] =
    pointedFunctor[({type λ[α]=Either.RightProjection[X, α]})#λ]

  implicit def EitherPointedFunctor[X] =
    pointedFunctor[({type λ[α]=Either[X, α]})#λ]

  import java.util.Map.Entry

  implicit def MapEntryPointedFunctor[X: Monoid] = {
    implicit val z = implicitly[Monoid[X]].zero
    implicit val s = implicitly[Monoid[X]].semigroup
    pointedFunctor[({type λ[α]=Entry[X, α]})#λ]
  }

  implicit def Tuple1PointedFunctor =
    pointedFunctor[Tuple1]

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

  implicit val IdentityPointedFunctor: PointedFunctor[Identity] =
    PointedFunctor.pointedFunctor[Identity]

  implicit def CoStatePointedFunctor[A: Zero, F[_] : PointedFunctor]: PointedFunctor[({type λ[α] = CoStateT[A, F, α]})#λ] = new PointedFunctor[({type λ[α] = CoStateT[A, F, α]})#λ] {
    implicit val ftr = implicitly[PointedFunctor[F]].functor
    implicit val pt = implicitly[PointedFunctor[F]].pointed
    val functor = implicitly[Functor[({type λ[α] = CoStateT[A, F, α]})#λ]]
    val pointed = implicitly[Pointed[({type λ[α] = CoStateT[A, F, α]})#λ]]
  }

  implicit def KleisliPointedFunctor[F[_], R](implicit pt: PointedFunctor[F]): PointedFunctor[({type λ[α] = Kleisli[R, F, α]})#λ] = {
    implicit val p = pt.pointed
    implicit val f = pt.functor
    PointedFunctor.pointedFunctor[({type λ[α] = Kleisli[R, F, α]})#λ]
  }

}
