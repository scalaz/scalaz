package scalaz
package typeclass

import Functor.syntax._
import Apply.syntax._
import Applicative.syntax._

trait TraversableInstances {
  implicit val list: Traversable[List] = new TraversableClass[List] {
    
    override def traverse[F[_], A, B](ta: List[A])(f: A => F[B])(implicit F: Applicative[F]): F[List[B]] =
      ta.foldLeft[F[List[B]]](List.empty[B].pure[F]) { (flb, a) => flb.ap(f(a).map(b => (xs: List[B]) => b::xs)) }
    
    override def sequence[F[_], A](ta: List[F[A]])(implicit F: Applicative[F]): F[List[A]] =
      traverse[F, F[A], A](ta)(identity)
  
    override def foldLeft[A, B](fa: List[A], z: B)(f: (B, A) => B): B = fa.foldLeft(z)(f)
    
    override def foldRight[A, B](fa: List[A], z: => B)(f: (A, => B) => B): B = fa.foldRight(z) { (a, b) => f(a, b) }
    
    override def toList[A](xs: List[A]): List[A] = xs

    override def map[A, B](fa: List[A])(f: A => B) = fa.map(f)
  }
}
