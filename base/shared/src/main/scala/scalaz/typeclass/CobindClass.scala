package scalaz
package typeclass

trait CobindClass[F[_]] extends Cobind[F] with FunctorClass[F] {
  final def cobind: Cobind[F] = this
}

object CobindClass {
  trait Cobind[F[_]] extends Alt[Cobind[F]] with CobindClass[F] {

    override def cojoin[A](fa: F[A]): F[F[A]] = cobind(fa)(a => a)

    override def cobind[A, B](fa: F[A])(f: F[A] => B): F[B]
  }

  trait Cojoin[F[_]] extends Alt[Cojoin[F]] with CobindClass[F] {

    override def cojoin[A](fa: F[A]): F[F[A]]

    override def cobind[A, B](fa: F[A])(f: F[A] => B): F[B] =
      map(cojoin(fa))(f)
  }

  trait Alt[D <: Alt[D]] { self: D => }

}
