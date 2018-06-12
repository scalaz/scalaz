package scalaz
package std

import scala.List
import scalaz.ct._

trait ListInstances {
  implicit val listCobind: Cobind[List] = instanceOf(new CobindClass.DeriveCojoin[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)

    override def cobind[A, B](fa: List[A])(f: List[A] => B): List[B] =
      List(f(fa))
  })

  implicit val listTraversable: Traversable[List] = instanceOf(
    new TraversableClass.DeriveSequence[List] with FoldableClass.DeriveFoldMap[List] {
      override def traverse[F[_], A, B](ta: List[A])(f: A => F[B])(implicit F: Applicative[F]): F[List[B]] =
        ta.foldLeft[F[List[B]]](F.pure(List.empty[B])) { (flb, a) =>
          {
            F.ap(flb)(F.map(f(a))(b => (xs: List[B]) => b :: xs))
          }
        }

      override def foldLeft[A, B](fa: List[A], z: B)(f: (B, A) => B): B = fa.foldLeft(z)(f)

      override def foldRight[A, B](fa: List[A], z: => B)(f: (A, => B) => B): B = fa.foldRight(z) { (a, b) =>
        f(a, b)
      }

      override def toList[A](xs: List[A]): List[A] = xs

      override def map[A, B](fa: List[A])(f: A => B) = fa.map(f)
    }
  )
}
