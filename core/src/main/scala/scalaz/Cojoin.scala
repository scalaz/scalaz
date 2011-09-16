package scalaz

trait CojoinLike[F[_]]  { self =>
  ////
  def cojoin[A](a: F[A]): F[F[A]]

  // derived functions

  ////
  val cojoinSyntax = new scalaz.syntax.CojoinSyntax[F] {}
}

////
/**
 *
 */
////
trait Cojoin[F[_]] extends CojoinLike[F]

trait CojoinInstance[F[_]] extends Cojoin[F]
