package scalaz
package std.java.util.concurrent

import java.util.concurrent.Callable


trait CallableInstances {
  implicit def callableEqual[A: Order] = new Order[Callable[A]] {
    def order(f1: Callable[A], f2: Callable[A]) = Order[A].order(f1.call, f2.call)
  }

  implicit def callableMonad: Monad[Callable] = new Monad[Callable] {
    def map[A, B](fa: Callable[A])(f: (A) => B) = new Callable[B] {
      def call() = f(fa.call)
    }
    def bind[A, B](fa: Callable[A])(f: (A) => Callable[B]) = new Callable[B] {
      def call() = f(fa.call).call
    }
    def point[A](a: => A) = new Callable[A] {
      def call() = a
    }
  }
}

object callable extends CallableInstances
