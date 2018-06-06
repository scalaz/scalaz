package scalaz
package std

import scala.{ None, Option, Some }

import core.EqClass
import data.Cord
import debug.DebugClass
import ct.MonadClass

trait OptionInstances {
  implicit val optionMonad: Monad[Option] =
    instanceOf(new MonadClass[Option] {
      override def pure[A](a: A): Option[A]                                   = Some(a)
      override def ap[A, B](fa: Option[A])(f: Option[A => B]): Option[B]      = fa.flatMap(a => f.map(fab => fab(a)))
      override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)
      override def flatten[A](ma: Option[Option[A]]): Option[A]               = ma.flatten
      override def map[A, B](ma: Option[A])(f: A => B): Option[B]             = ma.map(f)
    })

  implicit def optionEq[A](implicit X: Eq[A]): Eq[Option[A]] =
    instanceOf(new EqClass[Option[A]] {
      def equal(first: Option[A], second: Option[A]): Boolean = (first, second) match {
        case (None, None)       => true
        case (Some(a), Some(b)) => X.equal(a, b)
        case _                  => false
      }
    })

  implicit def optionDebug[A](implicit X: Debug[A]): Debug[Option[A]] = {
    import Scalaz.debugInterpolator
    DebugClass.instance[Option[A]] {
      case Some(a) => z"Some($a)"
      case None    => Cord("None")
    }
  }
}
