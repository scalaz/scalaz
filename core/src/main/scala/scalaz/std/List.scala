package scalaz
package std

trait Lists {
  implicit val list = new MonadPlus[List] with Traverse[List] with Empty[List] {
    def pure[A](a: => A): List[A] = scala.List(a)
    def bind[A,B](fa: List[A])(f: A => List[B]) = fa flatMap f
    def empty[A] = scala.List()
    def plus[A](a: List[A], b: => List[A]) = a ++ b
    override def map[A,B](l: List[A])(f: A => B) = l map f

    def traverseImpl[F[_],A,B](l: List[A])(f: A => F[B])(implicit F: Applicative[F]): F[List[B]] = 
      l.reverse.foldLeft(F.pure(scala.List[B]()))((fbl, a) => F.map2(f(a),fbl)(_ :: _))
    
    def foldR[A, B](fa: List[A], z: B)(f: (A) => (=> B) => B): B = fa.foldRight(z)((a, b) => f(a)(b))
  }
}

object List extends Lists
