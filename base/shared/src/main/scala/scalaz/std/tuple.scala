package scalaz
package std

import scala.{ List, Tuple2 }
import scalaz.ct._

trait TupleInstances {
  implicit final val tuple2Bifunctor: Bifunctor[Tuple2] =
    instanceOf(new BifunctorClass[Tuple2] with BifunctorClass.DeriveBimap[Tuple2] {
      def lmap[A, B, S](fab: (A, B))(f: A => S): (S, B) = fab.copy(_1 = f(fab._1))
      def rmap[A, B, T](fab: (A, B))(f: B => T): (A, T) = fab.copy(_2 = f(fab._2))
    })

  implicit def tuple2Cobind[A1]: Comonad[Tuple2[A1, ?]] =
    instanceOf(new ComonadClass[Tuple2[A1, ?]] with CobindClass.DeriveCojoin[Tuple2[A1, ?]] {
      override def map[A, B](fa: Tuple2[A1, A])(f: A => B): Tuple2[A1, B] = (fa._1, f(fa._2))

      override def cobind[A, B](fa: Tuple2[A1, A])(f: Tuple2[A1, A] => B): Tuple2[A1, B] =
        (fa._1, f(fa))

      override def copoint[A](fa: Tuple2[A1, A]): A = fa._2
    })

  implicit def tuple2Traversable[C]: Traversable[Tuple2[C, ?]] =
    instanceOf(new TraversableClass.DeriveSequence[Tuple2[C, ?]] with FoldableClass.DeriveFoldMap[Tuple2[C, ?]] {
      def traverse[F[_], A, B](ta: Tuple2[C, A])(f: A => F[B])(implicit F: Applicative[F]): F[Tuple2[C, B]] =
        F.map(f(ta._2))(b => (ta._1, b))

      override def foldLeft[A, B](ta: Tuple2[C, A], z: B)(f: (B, A) => B): B = f(z, ta._2)

      override def foldRight[A, B](ta: Tuple2[C, A], z: => B)(f: (A, => B) => B): B = f(ta._2, z)

      override def toList[A](ta: Tuple2[C, A]): List[A] = List(ta._2)

      override def map[A, B](ta: Tuple2[C, A])(f: A => B): Tuple2[C, B] = (ta._1, f(ta._2))
    })
}
