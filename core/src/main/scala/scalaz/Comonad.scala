package scalaz

trait ComonadLike[F[_]] extends Copointed[F] with Cojoin[F] { self =>
  ////

  // derived functions

  ////
  val comonadSyntax = new scalaz.syntax.ComonadSyntax[F] {}
}

////
/**
 *
 */
////
trait Comonad[F[_]] extends ComonadLike[F] {
  self  =>


}

object Comonad {
  def apply[F[_]](implicit F: Comonad[F]): Comonad[F] = F

  ////

  ////
}

trait ComonadInstance[F[_]] extends Comonad[F] with CopointedInstance[F] with CojoinInstance[F]
