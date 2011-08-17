package scalaz

trait Lists {
  implicit val list = new MonadPlusInstance[List] with TraverseInstance[List] {
    def pure[A](a: => A): List[A] = scala.List(a)
    def bind[A,B](fa: List[A])(f: A => List[B]) = fa flatMap f
    def empty[A] = scala.List()
    def plus[A](a: List[A], b: => List[A]) = a ++ b
    override def map[A,B](l: List[A])(f: A => B) = l map f
    def traverseImpl[F[_],A,B](l: List[A])(f: A => F[B])(implicit F: Applicative[F]): F[List[B]] = 
      l.foldRight(F.pure(scala.List[B]()))((a,fbl) => F.map2(f(a),fbl)(_ :: _))
  }
}

object List extends Lists
