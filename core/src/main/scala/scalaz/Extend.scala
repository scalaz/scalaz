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

  implicit def MapEntryExtend[X]: Extend[({type λ[α] = Entry[X, α]})#λ] =
    extend[({type λ[α] = Entry[X, α]})#λ]
}
