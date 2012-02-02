package scalaz

////
/**
 *
 */
////
trait AlternativeEmpty[F[_]] extends Alternative[F] { self =>
  ////
  def empty[A]: F[A]
  // derived functions

  trait EmptyLaw extends AlternativeLaw {
    def rightOrElseIdentity[A](f1: F[A])(implicit FA: Equal[F[A]]): Boolean =
      FA.equal(orElse(f1, empty[A]), f1)

    def leftOrElseIdentity[A](f1: F[A])(implicit FA: Equal[F[A]]): Boolean =
      FA.equal(orElse(empty[A], f1), f1)
  }

  def alternativeEmptyLaw = new EmptyLaw {}
  ////
  val alternativeEmptySyntax = new scalaz.syntax.AlternativeEmptySyntax[F] {}
}

object AlternativeEmpty {
  @inline def apply[F[_]](implicit F: AlternativeEmpty[F]): AlternativeEmpty[F] = F

  ////

  ////
}

