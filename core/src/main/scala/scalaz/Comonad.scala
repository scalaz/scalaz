package scalaz

trait ComonadLike[F[_]] extends CopointedLike[F] with CojoinLike[F] { self =>
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
  self : CopointedInstance[F] with CojoinInstance[F] =>

  implicit val comonadParents: CopointedInstance[F] with CojoinInstance[F] = this
}

object Comonad {
  def apply[F[_]](implicit F: Comonad[F]): Comonad[F] = F

  ////

  ////
}

trait ComonadInstance[F[_]] extends Comonad[F] with CopointedInstance[F] with CojoinInstance[F]
