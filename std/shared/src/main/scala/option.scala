package scalaz
package std

import scala.{ None, Option, Some }

import core.EqClass
import data.Cord
import debug.DebugClass
import ct._

trait OptionInstances {
  implicit val optionMonad: Monad[Option] =
    instanceOf(new MonadClass[Option] {
      def pure[A](a: A): Option[A]                                   = Some(a)
      def ap[A, B](fa: Option[A])(f: Option[A => B]): Option[B]      = fa.flatMap(a => f.map(fab => fab(a)))
      def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)
      def flatten[A](ma: Option[Option[A]]): Option[A]               = ma.flatten
      def map[A, B](ma: Option[A])(f: A => B): Option[B]             = ma.map(f)
    })

  implicit val optionCobind: Cobind[Option] = instanceOf(new CobindClass.DeriveCojoin[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)

    override def cobind[A, B](fa: Option[A])(f: Option[A] => B): Option[B] =
      Some(f(fa))
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
