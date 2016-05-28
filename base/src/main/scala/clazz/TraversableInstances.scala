package scalaz
package clazz

import Functor.syntax._
import Apply.syntax._
import Applicative.syntax._

trait TraversableInstances {
  implicit val list: Traversable[List] = new Traversable[List] {
    override val functor = Monad.list.applicative.apply.functor
    override val foldable = Foldable.list

    override def traverse[F[_], A, B](ta: List[A])(f: A => F[B])(implicit F: Applicative[F]): F[List[B]] =
      ta.foldLeft[F[List[B]]](List.empty[B].pure[F]) { (flb, a) => flb.ap(f(a).map(b => (xs: List[B]) => b::xs)) }
    override def sequence[F[_], A](ta: List[F[A]])(implicit F: Applicative[F]): F[List[A]] =
      traverse[F, F[A], A](ta)(identity)
  }
}
