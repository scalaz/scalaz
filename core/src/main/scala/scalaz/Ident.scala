package scalaz

trait Idents {
  implicit val id = new Monad[Id] {
    def pure[A](a: => A): A = a
    def bind[A,B](a: A)(f: A => B): B = f(a)

    // Overrides for efficiency.
    override def map[A, B](fa: Id[A])(f: (A) => B) = f(fa)

    override def map2[A, B, C](fa: Id[A], fb: Id[B])(f: (A, B) => C): Id[C] = f(fa, fb)

    override def apply[A, B](f: (A) => B): Id[A] => Id[B] = f

    override def ap[A, B](fa: Id[A])(f: Id[A => B]): Id[B] = f(fa)

    override def ap2[A, B, C](fa: Id[A], fb: Id[B])(f: Id[(A, B) => C]): Id[C] = f(fa, fb)

    override def lift2[A, B, C](f: (A, B) => C): (Id[A], Id[B]) => Id[C] = f

    // `ffa: Id[Id[A]]`, gives, "cyclic aliasing or subtyping involving type Id", but `fff: A` is identical.
    override def join[A](ffa: A) = ffa

    override def traverse[A, G[_], B](value: G[A])(f: A => Id[B])(implicit A: Applicative[Id], G: Traverse[G]): Id[G[B]] =
      G.map(value)(f)

    override def sequence[A, G[_]: Traverse](as: G[Id[A]]): Id[G[A]] = as

    // TODO more

  }
}

// Not named Id so as not to trigger https://issues.scala-lang.org/browse/SI-5031
object Ident extends Idents
