package scalaz
package tc

import scala.{ Function0, Tuple2 }

import scala.language.experimental.macros

trait ComonadClass[F[_]] extends CobindClass[F] {
  def copoint[A](fa: F[A]): A
}

object ComonadClass {
  implicit def tuple2Cobind[A1]: Comonad[Tuple2[A1, ?]] =
    instanceOf(new ComonadClass[Tuple2[A1, ?]] {
      override def map[A, B](fa: Tuple2[A1, A])(f: A => B): Tuple2[A1, B] = (fa._1, f(fa._2))

      override def cobind[A, B](fa: Tuple2[A1, A])(f: Tuple2[A1, A] => B): Tuple2[A1, B] =
        (fa._1, f(fa))

      override def copoint[A](fa: Tuple2[A1, A]): A = fa._2
    })

  implicit val function0Cobind: Comonad[Function0] =
    instanceOf(new ComonadClass[Function0] {
      override def map[A, B](fa: Function0[A])(f: A => B): Function0[B] = () => f(fa())

      override def cobind[A, B](fa: Function0[A])(f: Function0[A] => B): Function0[B] =
        () => f(fa)

      override def copoint[A](fa: Function0[A]): A = fa()
    })
}

trait ComonadSyntax {
  implicit final class ToComonadOps[F[_], A](self: F[A]) {
    def copoint(implicit ev: Comonad[F]): A = macro ops.Ops.i_0
  }
}
