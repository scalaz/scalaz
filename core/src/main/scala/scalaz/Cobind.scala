package scalaz

////
/**
 *
 */
////
trait CoBind[F[_]]  { self =>
  ////
  def cobind[A, B](fa: F[A])(f: F[A] => B): F[B]

  // derived functions

  ////
  val coBindSyntax = new scalaz.syntax.CoBindSyntax[F] {}
}

object CoBind {
  @inline def apply[F[_]](implicit F: CoBind[F]): CoBind[F] = F

  ////
  /** Define `CoBind` in terms of `CoJoin` and `Functor` */
  trait FromCoJoin[F[_]] extends CoBind[F]{
    self: CoJoin[F] with Functor[F] =>

    def cobind[A, B](fa: F[A])(f: (F[A]) => B): F[B] =
       map(cojoin(fa))(f)
  }
  ////
}

