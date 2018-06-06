package scalaz
package std

import scala.{ Either, Left, Right }

import core.EqClass
import ct.{ BifunctorClass, MonadClass }
import debug.DebugClass

trait EitherInstances {
  implicit def eitherMonad[L]: Monad[Either[L, ?]] =
    instanceOf(new MonadClass[Either[L, ?]] {
      override def pure[A](a: A): Either[L, A] = Right(a)
      override def ap[A, B](fa: Either[L, A])(f: Either[L, A => B]): Either[L, B] =
        fa.flatMap(a => f.map(fab => fab(a)))
      override def flatMap[A, B](ma: Either[L, A])(f: A => Either[L, B]): Either[L, B] = ma.flatMap(f)
      override def flatten[A](ma: Either[L, Either[L, A]]): Either[L, A]               = ma.flatMap(identity)
      override def map[A, B](ma: Either[L, A])(f: A => B): Either[L, B]                = ma.map(f)
    })

  implicit val eitherBifunctor: Bifunctor[Either] =
    instanceOf(new BifunctorClass[Either] {
      override def bimap[A, B, S, T](fab: Either[A, B])(as: A => S, bt: B => T): Either[S, T] = fab match {
        case Left(x)  => Left(as(x))
        case Right(x) => Right(bt(x))
      }
    })

  implicit def eitherEq[L, R](implicit X: Eq[L], Y: Eq[R]): Eq[Either[L, R]] =
    instanceOf(new EqClass[Either[L, R]] {
      def equal(first: Either[L, R], second: Either[L, R]): Boolean = (first, second) match {
        case (Left(x), Left(y))   => X.equal(x, y)
        case (Right(x), Right(y)) => Y.equal(x, y)
        case _                    => false
      }
    })

  implicit def eitherDebug[L, R](implicit L: Debug[L], R: Debug[R]): Debug[Either[L, R]] = {
    import Scalaz.debugInterpolator
    DebugClass.instance[Either[L, R]] {
      case Left(l)  => z"Left($l)"
      case Right(r) => z"Right($r)"
    }
  }
}
