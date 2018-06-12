package scalaz
package std

import scala.{ Option, Some }

import scalaz.ct._

trait OptionInstances {
  implicit val optionCobind: Cobind[Option] = instanceOf(new CobindClass.DeriveCojoin[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)

    override def cobind[A, B](fa: Option[A])(f: Option[A] => B): Option[B] =
      Some(f(fa))
  })
}
