package scalaz
package std

import scala.{ Either, Left, Right }

import core.EqAnyRef
import ct.{ BifunctorClass, MonadClass }
import debug.DebugClass

trait EitherInstances {
  implicit def eitherMonad[L]: Monad[Either[L, ?]] =
    instanceOf(new MonadClass[Either[L, ?]] {
      def pure[A](a: A): Either[L, A]                                         = Right(a)
      def ap[A, B](fa: Either[L, A])(f: Either[L, A => B]): Either[L, B]      = fa.flatMap(a => f.map(fab => fab(a)))
      def flatMap[A, B](ma: Either[L, A])(f: A => Either[L, B]): Either[L, B] = ma.flatMap(f)
      def flatten[A](ma: Either[L, Either[L, A]]): Either[L, A]               = ma.flatMap(identity)
      def map[A, B](ma: Either[L, A])(f: A => B): Either[L, B]                = ma.map(f)
    })

  implicit val eitherBifunctor: Bifunctor[Either] = instanceOf(new BifunctorClass[Either] {
    def bimap[A, B, S, T](fab: Either[A, B])(as: A => S, bt: B => T): Either[S, T] = fab match {
      case Left(x)  => Left(as(x))
      case Right(x) => Right(bt(x))
    }
    def lmap[A, B, S](fab: Either[A, B])(as: A => S): Either[S, B] = fab match {
      case Left(x) => Left(as(x))
      case _       => fab.asInstanceOf[Either[S, B]]
    }
    def rmap[A, B, T](fab: Either[A, B])(bt: B => T): Either[A, T] = fab match {
      case Right(x) => Right(bt(x))
      case _        => fab.asInstanceOf[Either[A, T]]
    }
  })

  implicit def eitherEq[L, R](implicit X: Eq[L], Y: Eq[R]): Eq[Either[L, R]] =
    instanceOf({
      case (Left(x), Left(y))   => X.equal(x, y)
      case (Right(x), Right(y)) => Y.equal(x, y)
      case _                    => false
    }: EqAnyRef[Either[L, R]])

  implicit def eitherDebug[L, R](implicit L: Debug[L], R: Debug[R]): Debug[Either[L, R]] = {
    import Scalaz.debugInterpolator
    DebugClass.instance[Either[L, R]] {
      case Left(l)  => z"Left($l)"
      case Right(r) => z"Right($r)"
    }
  }
}
