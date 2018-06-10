package scalaz
package ct

import scala.{ List, Tuple2 }

import scalaz.data.{ IList, Maybe2 }

import scala.language.experimental.macros

trait TraversableClass[T[_]] extends FunctorClass[T] with FoldableClass[T] {

  def traverse[F[_]: Applicative, A, B](ta: T[A])(f: A => F[B]): F[T[B]]

  def sequence[F[_]: Applicative, A](ta: T[F[A]]): F[T[A]]
}

object TraversableClass {

  trait DeriveSequence[T[_]] extends TraversableClass[T] with Alt[DeriveSequence[T]] {
    final override def sequence[F[_]: Applicative, A](ta: T[F[A]]): F[T[A]] = traverse(ta)(identity)
  }

  trait DeriveTraverse[T[_]] extends TraversableClass[T] with Alt[DeriveTraverse[T]] {
    final override def traverse[F[_]: Applicative, A, B](ta: T[A])(f: A => F[B]): F[T[B]] = sequence(map(ta)(f))
  }

  trait Alt[D <: Alt[D]]
}

trait TraversableFunctions {
  def sequence[T[_], F[_], A](tfa: T[F[A]])(implicit F: Applicative[F], T: Traversable[T]): F[T[A]] =
    T.sequence(tfa)
}

trait TraversableInstances {
  implicit val ilistTraversable: Traversable[IList] = instanceOf(
    new TraversableClass.DeriveSequence[IList] with FoldableClass.DeriveFoldMap[IList] {
      override def traverse[F[_], A, B](ta: IList[A])(f: A => F[B])(implicit F: Applicative[F]): F[IList[B]] =
        foldRight(ta, F.pure(IList.empty[B])) { (a, flb) =>
          F.ap(flb)(F.map(f(a))(b => IList.cons(b, _)))
        }

      @scala.annotation.tailrec
      override def foldLeft[A, B](fa: IList[A], z: B)(f: (B, A) => B): B = IList.uncons(fa) match {
        case Maybe2.Just2(a, as) => foldLeft(as, f(z, a))(f)
        case Maybe2.Empty2()     => z
      }

      def foldRightL[A, B](fa: IList[A], z: => B)(f: (A, => B) => B): () => B = IList.uncons(fa) match {
        // because `f` is lazy in its second argument, we don't stack-overflow.
        case Maybe2.Just2(a, as) => () => f(a, foldRightL(as, z)(f)())
        case Maybe2.Empty2()     => () => z
      }

      override def foldRight[A, B](fa: IList[A], z: => B)(f: (A, => B) => B): B = foldRightL[A, B](fa, z)(f)()

      override def toList[A](xs: IList[A]): List[A] = foldRight(xs, List.empty[A])(_ :: _)

      override def map[A, B](fa: IList[A])(f: A => B) = foldRight(fa, IList.empty[B])((a, bs) => IList.cons(f(a), bs))
    }
  )

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

trait TraversableSyntax {
  implicit final class ToTraversableOps[T[_], A](self: T[A]) {
    def traverse[F[_], B](f: A => F[B])(implicit g: Applicative[F], ev: Traversable[T]): F[T[B]] =
      macro meta.Ops.i_1_1i
  }
}
