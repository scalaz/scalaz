package scalaz
package ct

import scala.{ List, Option, Some }

trait CobindInstances {
  implicit val optionCobind: Cobind[Option] = instanceOf(new CobindClass.DeriveCojoin[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)

    override def cobind[A, B](fa: Option[A])(f: Option[A] => B): Option[B] =
      Some(f(fa))
  })

  implicit val listCobind: Cobind[List] = instanceOf(new CobindClass.DeriveCojoin[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)

    override def cobind[A, B](fa: List[A])(f: List[A] => B): List[B] =
      List(f(fa))
  })
}
