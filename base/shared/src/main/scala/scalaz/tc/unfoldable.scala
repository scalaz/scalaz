package scalaz
package tc

import scalaz.data.{ IList, Maybe, Maybe2 }

trait UnfoldableClass[F[_]] extends Unfoldable1Class[F] {
  def unfoldRight[A, B](f: B => Maybe2[A, B])(b: B): F[A]

  def unfoldRight1[A, B](f: B => (A, Maybe[B]))(b: B): F[A] =
    unfoldRight[A, B](
      b =>
        f(b) match {
          case (a, Maybe.Just(b1)) => Maybe2.just2(a, b1)
          case (_, Maybe.Empty())  => Maybe2.empty2
      }
    )(b)

  def fromList[A](as: IList[A]): F[A] = unfoldRight(IList.uncons[A])(as)
}
