package scalaz
package std.java.util.concurrent

import java.util.concurrent.Callable


trait CallableInstances {
  implicit def callableOrder[A: Order]: Order[Callable[A]] =
    (f1: Callable[A], f2: Callable[A]) => Order[A].order(f1.call, f2.call)

  implicit val callableMonad: Monad[Callable] = new Monad[Callable] {
    override def map[A, B](fa: Callable[A])(f: A => B) = () => f(fa.call)
    def bind[A, B](fa: Callable[A])(f: A => Callable[B]) = () => f(fa.call).call
    def point[A](a: => A) = () => a
  }
}

object callable extends CallableInstances
