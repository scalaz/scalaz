package scalaz

////
/**
 *
 */
////
trait CoJoin[F[_]] extends Functor[F]  { self =>
  ////
  /** Also known as `duplicate` */
  def cojoin[A](a: F[A]): F[F[A]]

  // derived functions

  ////
  val coJoinSyntax = new scalaz.syntax.CoJoinSyntax[F] {}
}

object CoJoin {
  @inline def apply[F[_]](implicit F: CoJoin[F]): CoJoin[F] = F

  ////
  /** Define `CoJoin` in terms of `CoBind` */
  trait FromCoBind[F[_]] extends CoJoin[F] {
    self: CoBind[F] =>

    def cojoin[A](fa: F[A]): F[F[A]] = cobind(fa)(fa => fa)
  }

  ////
}

