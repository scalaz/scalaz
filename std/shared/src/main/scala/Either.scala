package scalaz
package std

import scalaz.typeclass.{ EqClass, MonadClass }

trait EitherInstances {
  implicit def eitherMonad[L]: Monad[Either[L, ?]] =
    instanceOf(new MonadClass[Either[L, ?]] {
      def pure[A](a: A): Either[L, A]                                         = Right(a)
      def ap[A, B](fa: Either[L, A])(f: Either[L, A => B]): Either[L, B]      = fa.flatMap(a => f.map(fab => fab(a)))
      def flatMap[A, B](ma: Either[L, A])(f: A => Either[L, B]): Either[L, B] = ma.flatMap(f)
      def flatten[A](ma: Either[L, Either[L, A]]): Either[L, A]               = ma.flatMap(identity)
      def map[A, B](ma: Either[L, A])(f: A => B): Either[L, B]                = ma.map(f)
    })

  implicit def eitherEq[L, R](implicit X: Eq[L], Y: Eq[R]): Eq[Either[L, R]] =
    instanceOf(new EqClass[Either[L, R]] {
      def equal(first: Either[L, R], second: Either[L, R]): Boolean = (first, second) match {
        case (Left(x), Left(y))   => X.equal(x, y)
        case (Right(x), Right(y)) => Y.equal(x, y)
        case _                    => false
      }
    })
}
