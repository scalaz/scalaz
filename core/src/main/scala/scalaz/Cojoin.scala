package scalaz

////
/**
 *
 */
////
trait Cojoin[F[_]]  { self =>
  ////
  /** Also known as `duplicate` */
  def cojoin[A](a: F[A]): F[F[A]]

  // derived functions

  ////
  val cojoinSyntax = new scalaz.syntax.CojoinSyntax[F] {}
}

object Cojoin {
  @inline def apply[F[_]](implicit F: Cojoin[F]): Cojoin[F] = F

  ////
  /** Define `Cojoin` in terms of `Cobind` */
  trait FromCobind[F[_]] extends Cojoin[F] {
    self: Cobind[F] =>

    def cojoin[A](fa: F[A]): F[F[A]] = cobind(fa)(fa => fa)
  }

  ////
}

