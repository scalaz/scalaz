package scalaz

trait Cobind[F[_]]  { self =>
  ////
  def cobind[A, B](fa: F[A])(f: F[A] => B): F[B]

  // derived functions

  ////
  val cobindSyntax = new scalaz.syntax.CobindSyntax[F] {}
}

object Cobind {
  def apply[F[_]](implicit F: Cobind[F]): Cobind[F] = F

  ////

  trait FromCojoin[F[_]] extends Cobind[F]{
    self: Cojoin[F] with Functor[F] =>

    def cobind[A, B](fa: F[A])(f: (F[A]) => B): F[B] =
       map(cojoin(fa))(f)
  }
  ////
}

