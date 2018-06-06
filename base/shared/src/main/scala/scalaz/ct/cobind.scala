package scalaz
package ct

import scala.{ List, Option, Some }

import scala.language.experimental.macros

@meta.minimal("cobind", "cojoin")
trait CobindClass[F[_]] extends FunctorClass[F] {

  def cobind[A, B](fa: F[A])(f: F[A] => B): F[B] = map(cojoin(fa))(f)

  def cojoin[A](fa: F[A]): F[F[A]] = cobind(fa)(identity)
}

trait CobindInstances {
  implicit val optionCobind: Cobind[Option] = instanceOf(new CobindClass[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)

    override def cobind[A, B](fa: Option[A])(f: Option[A] => B): Option[B] =
      Some(f(fa))
  })

  implicit val listCobind: Cobind[List] = instanceOf(new CobindClass[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)

    override def cobind[A, B](fa: List[A])(f: List[A] => B): List[B] =
      List(f(fa))
  })
}

trait CobindSyntax {
  implicit final class ToCobindOps[F[_], A](fa: F[A]) {
    def cobind[B](f: F[A] => B)(implicit ev: Cobind[F]): F[B] = macro meta.Ops.i_1
  }
}
