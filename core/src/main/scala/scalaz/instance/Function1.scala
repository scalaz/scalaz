package scalaz
package instance


trait Function1s {
  implicit def function1Apply[T] = new MonadInstance[({type l[a] = (T => a)})#l] {
    def pure[A](a: => A) = Function.const(a)

    def bind[A, B](fa: T => A)(f: A => T => B): (T => B) = (t: T) => f(fa(t))(t)
  }
}

object Function1 extends Function1s 
