package scalaz

////
/**
 *
 */
////
trait Comonad[F[_]] extends Cobind[F] { self =>
  ////
  /** Also known as `extract` / `copure` */
  def copoint[A](p: F[A]): A

  // derived functions

  /** alias for `copoint` */
  def copure[A](p: F[A]): A = copoint(p)

  trait ComonadLaws extends CobindLaws {
    def cobindLeftIdentity[A](fa: F[A])(implicit F: Equal[F[A]]): Boolean =
      F.equal(cobind(fa)(copoint), fa)
    def cobindRightIdentity[A, B](fa: F[A], f: F[A] => B)(implicit F: Equal[B]): Boolean =
      F.equal(copoint(cobind(fa)(f)), f(fa))
  }

  def comonadLaw = new ComonadLaws {}

  ////
  val comonadSyntax = new scalaz.syntax.ComonadSyntax[F] { def F = Comonad.this }
}

object Comonad {
  @inline def apply[F[_]](implicit F: Comonad[F]): Comonad[F] = F

  ////

  ////
}
