package scalaz

/** Mixed into object `scalaz.Id` in the package object [[scalaz]]. */
trait IdInstances {
  implicit val id: Traverse[Id] with Monad[Id] with CoMonad[Id] with CoJoin[Id] = new Traverse[Id] with Monad[Id] with CoMonad[Id] with CoBind.FromCoJoin[Id] {
    def point[A](a: => A): A = a
    def bind[A,B](a: A)(f: A => B): B = f(a)
    def cojoin[A](a: Id[A]): A = a
    def copoint[A](p: Id[A]): A = p
    def traverseImpl[G[_]: Applicative, A, B](fa: Id[A])(f: (A) => G[B]): G[Id[B]] = f(fa)
    def foldRight[A, B](fa: scalaz.Id[A], z: => B)(f: (A, => B) => B): B = f(fa, z)

    // Overrides for efficiency.

    override def apply[A, B](f: (A) => B): Id[A] => Id[B] = f

    // `ffa: Id[Id[A]]`, gives, "cyclic aliasing or subtyping involving type Id", but `ffa: A` is identical.
    override def join[A](ffa: A) = ffa

    override def traverse[A, G[_], B](value: G[A])(f: A => Id[B])(implicit G: Traverse[G]): Id[G[B]] = G.map(value)(f)

    override def sequence[A, G[_]: Traverse](as: G[Id[A]]): Id[G[A]] = as

    override def ap[A, B](fa: => Id[A])(f: => Id[A => B]): Id[B] = f(fa)

    /*TODO Bring back after Apply is remodelled.
    override def ap2[A, B, C](fa: Id[A], fb: Id[B])(f: Id[(A, B) => C]): Id[C] = f(fa, fb)

    override def ap3[A, B, C, D](fa: Id[A], fb: Id[B], fc: Id[C])(f: Id[(A, B, C) => D]): Id[D] = f(fa, fb, fc)

    override def ap4[A, B, C, D, E](fa: Id[A], fb: Id[B], fc: Id[C], fd: Id[D])(f: Id[(A, B, C, D) => E]): Id[E] = f(fa, fb, fc, fd)

    override def ap5[A, B, C, D, E, R](fa: Id[A], fb: Id[B], fc: Id[C], fd: Id[D], fe: Id[E])(f: Id[(A, B, C, D, E) => R]): Id[R] = f(fa, fb, fc, fd, fe)

    override def map[A, B](fa: Id[A])(f: (A) => B) = f(fa)

    override def map2[A, B, C](fa: Id[A], fb: Id[B])(f: (A, B) => C): Id[C] = f(fa, fb)

    override def map3[A, B, C, D](fa: Id[A], fb: Id[B], fc: Id[C])(f: (A, B, C) => D): Id[D] = f(fa, fb, fc)

    override def map4[A, B, C, D, E](fa: Id[A], fb: Id[B], fc: Id[C], fd: Id[D])(f: (A, B, C, D) => E): Id[E] = f(fa, fb, fc, fd)

    override def map5[A, B, C, D, E, R](fa: Id[A], fb: Id[B], fc: Id[C], fd: Id[D], fe: Id[E])(f: (A, B, C, D, E) => R): Id[R] = f(fa, fb, fc, fd, fe)

    override def lift2[A, B, C](f: (A, B) => C): (Id[A], Id[B]) => Id[C] = f

    override def lift3[A, B, C, D](f: (A, B, C) => D): (Id[A], Id[B], Id[C]) => Id[D] = f

    override def lift4[A, B, C, D, E](f: (A, B, C, D) => E): (Id[A], Id[B], Id[C], Id[D]) => Id[E] = f

    override def lift5[A, B, C, D, E, R](f: (A, B, C, D, E) => R): (Id[A], Id[B], Id[C], Id[D], Id[E]) => Id[R] = f*/

    override def compose[G[_]](G0: Applicative[G]): Applicative[G] = G0

    // TODO Fun compiler bug? "can't existentially abstract over parameterized type G"
    // override def product1[G[_]](implicit G0: Applicative[G]): Applicative[G] = G0
  }
}
