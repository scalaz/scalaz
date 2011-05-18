package scalaz

import java.util.Map.Entry

trait CoPointedFunctor[F[_]] {
  val functor: Functor[F]
  val coPointed: CoPointed[F]

  def fmap[A, B](f: A => B): F[A] => F[B] =
    functor.fmap(f)

  def coPoint[A]: F[A] => A =
    coPointed.coPoint[A]
}

object CoPointedFunctor extends CoPointedFunctors

trait CoPointedFunctors {
  implicit def coPointedFunctor[F[_]](implicit t: Functor[F], c: CoPointed[F]): CoPointedFunctor[F] = new CoPointedFunctor[F] {
    val functor = t
    val coPointed = c
  }

  implicit def MapEntryCoPointedFunctor[X]: CoPointedFunctor[({type λ[α] = Entry[X, α]})#λ] =
    coPointedFunctor[({type λ[α] = Entry[X, α]})#λ]
}
