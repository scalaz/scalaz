package scalaz

package tc

package instances

import scala.List

object list {

  def data[A]: MonoidClass[List[A]] =
    new MonoidClass[List[A]] {
      def mappend(a1: List[A], a2: => List[A]): List[A] = a1 ++ a2
      def mempty: List[A]                               = List.empty
    }

  val control: CobindClass[List] with MonadClass[List] with TraversableClass[List] =
    new CobindClass[List] with MonadClass[List] with TraversableClass[List] {
      override def ap[A, B](xs: List[A])(f: List[A => B]): List[B]      = xs.flatMap(a => f.map(_(a)))
      override def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] = xs.flatMap(f)
      override def map[A, B](xs: List[A])(f: A => B): List[B]           = xs.map(f)
      override def pure[A](a: A): List[A]                               = List(a)
      override def cobind[A, B](fa: List[A])(f: List[A] => B): List[B]  = List(f(fa))

      override def traverse[F[_], A, B](ta: List[A])(f: A => F[B])(implicit F: Applicative[F]): F[List[B]] =
        ta.reverse.foldLeft(F.pure(List.empty[B])) { (flb, a) =>
          F.ap(f(a))(F.map(flb)(lb => (b: B) => b :: lb))
        }

      override def foldLeft[A, B](fa: List[A], z: B)(f: (B, A) => B): B =
        fa.foldLeft(z)(f)

      override def foldRight[A, B](fa: List[A], z: => B)(f: (A, => B) => B): B =
        fa.foldRight(z) { (a, b) =>
          f(a, b)
        }

      override def toList[A](xs: List[A]): List[A] = xs
    }
}
