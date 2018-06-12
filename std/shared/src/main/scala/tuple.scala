package scalaz
package std

import scala.{ List, Tuple1, Tuple2 }

import scalaz.core.EqClass
import scalaz.ct._

trait TupleInstances {
  implicit final def tuple1Eq[A](implicit A: Eq[A]): Eq[Tuple1[A]] =
    instanceOf[EqClass[Tuple1[A]]] {
      case (Tuple1(a1), Tuple1(a2)) => A.equal(a1, a2)
      case _                        => false
    }

  implicit final def tuple2Eq[A, B](implicit A: Eq[A], B: Eq[B]): Eq[Tuple2[A, B]] =
    instanceOf[EqClass[Tuple2[A, B]]] {
      case ((a1, b1), (a2, b2)) => A.equal(a1, a2) && B.equal(b1, b2)
      case _                    => false
    }

  implicit final def tuple3Eq[A, B, C](implicit A: Eq[A], B: Eq[B], C: Eq[C]): Eq[(A, B, C)] =
    instanceOf[EqClass[(A, B, C)]] {
      case ((a1, b1, c1), (a2, b2, c2)) =>
        A.equal(a1, a2) && B.equal(b1, b2) && C.equal(c1, c2)
      case _ => false
    }

  implicit final def tuple4Eq[A, B, C, D](implicit A: Eq[A], B: Eq[B], C: Eq[C], D: Eq[D]): Eq[(A, B, C, D)] =
    instanceOf[EqClass[(A, B, C, D)]] {
      case ((a1, b1, c1, d1), (a2, b2, c2, d2)) =>
        A.equal(a1, a2) && B.equal(b1, b2) && C.equal(c1, c2) && D.equal(d1, d2)
      case _ => false
    }

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
