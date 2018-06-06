package scalaz
package ct

import scala.{ List, Tuple2 }

import scala.language.experimental.macros

@meta.minimal("traverse", "sequence")
trait TraversableClass[T[_]] extends FunctorClass[T] with FoldableClass[T] {

  def traverse[F[_]: Applicative, A, B](ta: T[A])(f: A => F[B]): F[T[B]] =
    sequence[F, B](map(ta)(f))

  def sequence[F[_]: Applicative, A](ta: T[F[A]]): F[T[A]] =
    traverse(ta)(identity)
}

trait TraversableFunctions {
  def sequence[T[_], F[_], A](tfa: T[F[A]])(implicit F: Applicative[F], T: Traversable[T]): F[T[A]] =
    T.sequence(tfa)
}

trait TraversableInstances {
  implicit val listTraversable: Traversable[List] =
    instanceOf(new TraversableClass[List] {
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
    })

  implicit def tuple2Traversable[C]: Traversable[Tuple2[C, ?]] =
    instanceOf(new TraversableClass[Tuple2[C, ?]] {
      override def traverse[F[_], A, B](ta: Tuple2[C, A])(f: A => F[B])(implicit F: Applicative[F]): F[Tuple2[C, B]] =
        F.map(f(ta._2))(b => (ta._1, b))

      override def foldLeft[A, B](ta: Tuple2[C, A], z: B)(f: (B, A) => B): B = f(z, ta._2)

      override def foldRight[A, B](ta: Tuple2[C, A], z: => B)(f: (A, => B) => B): B = f(ta._2, z)

      override def toList[A](ta: Tuple2[C, A]): List[A] = List(ta._2)

      override def map[A, B](ta: Tuple2[C, A])(f: A => B): Tuple2[C, B] = (ta._1, f(ta._2))
    })
}

trait TraversableSyntax {
  implicit final class ToTraversableOps[T[_], A](self: T[A]) {
    def traverse[F[_], B](f: A => F[B])(implicit g: Applicative[F], ev: Traversable[T]): F[T[B]] =
      macro meta.Ops.i_1_1i
  }
}
