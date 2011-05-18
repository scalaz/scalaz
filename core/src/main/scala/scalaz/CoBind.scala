package scalaz

import java.util.Map.Entry

trait CoBind[F[_]] {
  def coBind[A, B](f: F[A] => B): F[A] => F[B]
}

object CoBind extends CoBinds

trait CoBinds {
  def coBind[F[_]](implicit e: Extend[F]): CoBind[F] = new CoBind[F] {
    def coBind[A, B](f: F[A] => B) =
      e.functor.fmap(f) compose e.coJoin.coJoin
  }

  implicit def MapEntryCoBind[X]: CoBind[({type λ[α] = Entry[X, α]})#λ] =
    coBind[({type λ[α] = Entry[X, α]})#λ]
}
