package scalaz
package instance

trait Options {
  implicit val option = new MonadPlusInstance[Option] with TraverseInstance[Option] {
    def pure[A](a: => A) = Some(a)
    def bind[A,B](fa: Option[A])(f: A => Option[B]): Option[B] = fa flatMap f
    override def map[A,B](fa: Option[A])(f: A => B): Option[B] = fa map f
    def traverseImpl[F[_],A,B](fa: Option[A])(f: A => F[B])(implicit F: Applicative[F]) = 
      fa map (a => F.map(f(a))(Some(_): Option[B])) getOrElse F.pure(None) 
    def empty[A]: Option[A] = None
    def plus[A](a: Option[A], b: => Option[A]) = a orElse b
  }
}

object Option extends Options

