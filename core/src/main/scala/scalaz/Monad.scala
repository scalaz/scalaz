package scalaz

trait Monad[M[_]] extends Applicative[M] with Bind[M] with Pointed[M] {
  override def fmap[A, B](fa: M[A], f: A => B) = bind(fa, (a: A) => pure(f(a)))
  override def apply[A, B](f: M[A => B], a: M[A]): M[B] = bind(f, (k: A => B) => fmap(a, k(_: A)))
}

object Monad {
  implicit def monad[M[_]](implicit b: Bind[M], p: Pure[M]): Monad[M] = new Monad[M] {
    override def pure[A](a: => A) = p.pure(a)
    override def bind[A, B](a: M[A], f: A => M[B]) = b.bind(a, f)
  }
}
