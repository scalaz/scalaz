package scalaz

////
/**
 *
 */
////
trait Cobind[F[_]] extends Functor[F] { self =>
  ////
  /** Also know as `extend` */
  def cobind[A, B](fa: F[A])(f: F[A] => B): F[B]

  // derived functions

  ////
  val cobindSyntax = new scalaz.syntax.CobindSyntax[F] { def F = Cobind.this }
}

object Cobind {
  @inline def apply[F[_]](implicit F: Cobind[F]): Cobind[F] = F

  ////
  /** Define `Cobind` in terms of `Cojoin` and `Functor` */
  trait FromCojoin[F[_]] extends Cobind[F]{
    self: Cojoin[F] =>

    def cobind[A, B](fa: F[A])(f: (F[A]) => B): F[B] = map(cojoin(fa))(f)
  }

  ////
}
