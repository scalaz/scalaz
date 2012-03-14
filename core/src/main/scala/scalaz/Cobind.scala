package scalaz

////
/**
 *
 */
////
trait CoBind[F[_]] extends Functor[F]  { self =>
  ////
  /** Also know as `extend` */
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
    self: CoJoin[F] =>

    def cobind[A, B](fa: F[A])(f: (F[A]) => B): F[B] =
       map(cojoin(fa))(f)
  }
  ////
}

