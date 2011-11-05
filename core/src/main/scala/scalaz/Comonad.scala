package scalaz

trait CoMonad[F[_]] extends CoPointed[F] with CoJoin[F] with CoBind[F] { self =>
  ////

  // derived functions

  ////
  val coMonadSyntax = new scalaz.syntax.CoMonadSyntax[F] {}
}

object CoMonad {
  def apply[F[_]](implicit F: CoMonad[F]): CoMonad[F] = F

  ////

  ////
}

