package scalaz
package typeclass

import Prelude._
import FoldableClass._
import TraversableClass._

trait TraversableInstances {
  implicit val list: Traversable[List] = new TraversableClass[List] with Traverse[List] with FoldRight[List] {

    override def traverse[F[_], A, B](ta: List[A])(f: A => F[B])(implicit F: Applicative[F]): F[List[B]] =
      ta.foldLeft[F[List[B]]](List.empty[B].pure[F]) { (flb, a) => flb.ap(f(a).map(b => (xs: List[B]) => b::xs)) }

    override def foldLeft[A, B](fa: List[A], z: B)(f: (B, A) => B): B = fa.foldLeft(z)(f)

    override def foldRight[A, B](fa: List[A], z: => B)(f: (A, => B) => B): B = fa.foldRight(z) { (a, b) => f(a, b) }

    override def toList[A](xs: List[A]): List[A] = xs

    override def map[A, B](fa: List[A])(f: A => B) = fa.map(f)
  }

  implicit def tuple2[C]: Traversable[Tuple2[C, ?]] = new TraversableClass[Tuple2[C, ?]] with Traverse[Tuple2[C, ?]] with FoldRight[Tuple2[C, ?]] {
    override def traverse[F[_], A, B](ta: Tuple2[C, A])(f: A => F[B])(implicit F: Applicative[F]): F[Tuple2[C, B]] =
      f(ta._2).map(b => (ta._1, b))

    override def foldLeft[A, B](ta: Tuple2[C, A], z: B)(f: (B, A) => B): B = f(z, ta._2)

    override def foldRight[A, B](ta: Tuple2[C, A], z: => B)(f: (A, => B) => B): B = f(ta._2, z)

    override def toList[A](ta: Tuple2[C, A]): List[A] = List(ta._2)

    override def map[A, B](ta: Tuple2[C, A])(f: A =>B): Tuple2[C, B] = (ta._1, f(ta._2))
  }
}
