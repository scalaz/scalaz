package scalaz

import data._, Ident._

trait Traverse[T[_]] {
  def traverse[F[_] : Applicative, A, B](f: A => F[B]): T[A] => F[T[B]]

  def functor: Functor[T] = new Functor[T] {
    def fmap[A, B](f: A => B) = t => {
      val k = traverse[Ident, A, B](a => ident(f(a)))
      k(t).value
    }
  }

  def fmap[A, B](f: A => B): T[A] => T[B] =
    functor.fmap(f)
}

object Traverse extends Traverses

trait Traverses {

  implicit def StreamTraverse: Traverse[Stream] = new Traverse[Stream] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B]) =
      implicitly[Foldr[Stream]].foldr[A, F[Stream[B]]](x => ys => error(""))(implicitly[Applicative[F]].point(Stream.Empty))

    // _.foldr[F[Stream[B]]]((Stream.Empty: Stream[B]) η, (x, ys) => a(f(x) ∘ ((a: B) => (b: Stream[B]) => a #:: b), ys))
  }
}