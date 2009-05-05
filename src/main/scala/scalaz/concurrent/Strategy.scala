package scalaz.concurrent

import scalaz.Functor

trait Strategy[A] {
  def apply(a: () => A): () => A
}

object Strategy {
  implicit def strategyFrom[A](f: (() => A) => () => A) = new Strategy[A] {
    def apply(a: () => A) = f(a)
  }

  implicit def strategyTo[A](s: Strategy[A]) = (a: () => A) => s(a)
  
  def parM[M[_], A](as: M[() => A])(implicit m: Functor[M], s: Strategy[A]) = m.fmap(as, s)
}