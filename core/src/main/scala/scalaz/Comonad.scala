package scalaz

////
/**
 *
 */
////
trait Comonad[F[_]] extends Copointed[F] with Cojoin[F] with Cobind[F] { self =>
  ////

  // derived functions
  trait ComonadLaws {
    def cobindLeftIdentity[A](fa: F[A])(implicit F: Equal[F[A]]): Boolean =
      F.equal(cobind(fa)(copoint), fa)
    def cobindRightIdentity[A, B](fa: F[A], f: F[A] => B)(implicit F: Equal[B]): Boolean =
      F.equal(copoint(cobind(fa)(f)), f(fa))
    def cobindAssociative[A, B, C, D](fa: F[A], f: F[A] => B, g: F[B] => C, h: F[C] => D)(implicit F: Equal[D]): Boolean = {
      implicit val C = self
      val d1 = (CoKleisli(f) =>= CoKleisli(g) =>= CoKleisli(h)) run fa
      val d2 = (CoKleisli(f) =>= (CoKleisli(g) =>= CoKleisli(h))) run fa
      F.equal(d1, d2)
    }

  }

  def comonadLaw = new ComonadLaws {}

  ////
  val comonadSyntax = new scalaz.syntax.ComonadSyntax[F] {}
}

object Comonad {
  @inline def apply[F[_]](implicit F: Comonad[F]): Comonad[F] = F

  ////

  ////
}

