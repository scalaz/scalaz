package scalaz

import java.util.Map.Entry

trait Extend[F[_]] {
  val functor: Functor[F]
  val coJoin: CoJoin[F]

  def fmap[A, B](f: A => B): F[A] => F[B] =
    functor.fmap(f)

  def coJn[A]: F[A] => F[F[A]] =
    coJoin.coJoin[A]

  def coBind: CoBind[F] = new CoBind[F] {
    def coBind[A, B](f: F[A] => B) =
      a =>
        functor.fmap((z: F[A]) => f(z))(coJoin.coJoin(a))
  }
}

object Extend extends Extends

trait Extends {
  def extend[F[_]](implicit j: CoJoin[F], f: Functor[F]): Extend[F] = new Extend[F] {
    val functor = f
    val coJoin = j
  }

  implicit val ListExtend: Extend[List] =
    extend[List]

  implicit val OptionExtend: Extend[Option] =
    extend[Option]

  implicit def EitherLeftExtend[X]: Extend[({type λ[α] = Either.LeftProjection[α, X]})#λ] =
    extend[({type λ[α] = Either.LeftProjection[α, X]})#λ]

  implicit def EitherRightExtend[X]: Extend[({type λ[α] = Either.RightProjection[X, α]})#λ] =
    extend[({type λ[α] = Either.RightProjection[X, α]})#λ]

  implicit def EitherExtend[X]: Extend[({type λ[α] = Either[X, α]})#λ] =
    extend[({type λ[α] = Either[X, α]})#λ]

  implicit def Function1Extend[R: Semigroup]: Extend[({type λ[α] = (R => α)})#λ] =
    extend[({type λ[α] = (R => α)})#λ]

  implicit def MapEntryExtend[X]: Extend[({type λ[α] = Entry[X, α]})#λ] =
    extend[({type λ[α] = Entry[X, α]})#λ]

  implicit def CoStateExtend[A, F[_] : Extend]: Extend[({type λ[α] = CoStateT[A, F, α]})#λ] = new Extend[({type λ[α] = CoStateT[A, F, α]})#λ] {
    implicit val ftr = implicitly[Extend[F]].functor
    implicit val cb = implicitly[Extend[F]].coBind
    val functor = implicitly[Functor[({type λ[α] = CoStateT[A, F, α]})#λ]]
    val coJoin = implicitly[CoJoin[({type λ[α] = CoStateT[A, F, α]})#λ]]
  }
}
