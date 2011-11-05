package scalaz

trait CoJoin[F[_]]  { self =>
  ////
  def cojoin[A](a: F[A]): F[F[A]]

  // derived functions

  ////
  val coJoinSyntax = new scalaz.syntax.CoJoinSyntax[F] {}
}

object CoJoin {
  def apply[F[_]](implicit F: CoJoin[F]): CoJoin[F] = F

  ////
  /** Define `CoJoin` in terms of `CoBind` */
  trait FromCoBind[F[_]] extends CoJoin[F] {
    self: CoBind[F]

    def cojoin[A](fa: F[A]): F[F[A]] = cobind(fa)(fa => fa)
  }

  ////
}

