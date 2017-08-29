package scalaz
package typeclass

trait ComonadClass[F[_]] extends Comonad[F] with CobindClass[F] with FunctorClass[F] {
  final def comonad: Comonad[F] = this
}

object ComonadClass {
  trait Cobind[F[_]] extends Alt[Cobind[F]] with ComonadClass[F] {
    override def copoint[A](fa: F[A]): A

    override def cojoin[A](fa: F[A]): F[F[A]] = cobind(fa)(a => a)

    override def cobind[A, B](fa: F[A])(f: F[A] => B): F[B]
  }

  trait Cojoin[F[_]] extends Alt[Cojoin[F]] with ComonadClass[F] {
    override def copoint[A](fa: F[A]): A

    override def cojoin[A](fa: F[A]): F[F[A]]

    override def cobind[A, B](fa: F[A])(f: F[A] => B): F[B] =
      map(cojoin(fa))(f)
  }

  trait Alt[D <: Alt[D]] { self: D => }
}
