package scalaz
package std.util.concurrent

import java.util.concurrent.Callable


trait CallableInstances {
  implicit def callableEqual[A: Order] = new Order[Callable[A]] {
    def order(f1: Callable[A], f2: Callable[A]) = Order[A].order(f1.call, f2.call)
  }

  implicit def callableFunctor[A] = new Functor[Callable] with Pointed[Callable] {
    def map[A, B](fa: Callable[A])(f: (A) => B) = new Callable[B] {
      def call() = f(fa.call)
    }

    def point[A](a: => A) = new Callable[A] {
      def call() = a
    }
  }
}

object callable extends CallableInstances
