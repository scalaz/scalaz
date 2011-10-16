package scalaz
package std

trait Streams {
  implicit object stream extends Traverse[Stream] with MonadPlus[Stream] {
    def traverseImpl[G[_] : Applicative, A, B](fa: Stream[A])(f: (A) => G[B]): G[Stream[B]] = {
      val G = Applicative[G]
      val seed: G[Stream[B]] = G.pure(scala.Stream.empty[B])
      foldR(fa, seed) {
        x => ys =>
          G.ap(ys)(G.map(f(x))((a: B) => (b: Stream[B]) => a #:: b))
      }
    }

    def foldR[A, B](fa: Stream[A], z: B)(f: (A) => (=> B) => B): B = if (fa.isEmpty)
      z
    else
      f(fa.head)(foldR(fa.tail, z)(f))

    def bind[A, B](fa: Stream[A])(f: (A) => Stream[B]): Stream[B] = fa flatMap f
    def empty[A]: Stream[A] = scala.Stream.empty
    def plus[A](a: Stream[A], b: => Stream[A]): Stream[A] = a #::: b
    def pure[A](a: => A): Stream[A] = scala.Stream(a)
  }

  implicit def streamMonoid[A] = new Monoid[Stream[A]] {
    def append(f1: Stream[A], f2: => Stream[A]): Stream[A] = f1 #::: f2
    def zero: Stream[A] = scala.Stream.empty
  }

  // TODO show, equal, order, ...
}

object Stream extends Streams