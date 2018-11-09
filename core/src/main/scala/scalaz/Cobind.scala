package scalaz

////
/**
 *
 */
////
trait Cobind[F[_]] extends Functor[F] { self =>
  ////
  /** Also know as `extend` */
  def cobind[A, B](fa: F[A])(f: F[A] => B): F[B]

  final def extend[A, B](fa: F[A])(f: F[A] => B): F[B] = cobind(fa)(f)

  /** Also known as `duplicate` */
  def cojoin[A](fa: F[A]): F[F[A]] =
    cobind(fa)(fa => fa)

  // derived functions

  trait CobindLaws {
    def cobindAssociative[A, B, C, D](fa: F[A], f: F[A] => B, g: F[B] => C, h: F[C] => D)(implicit F: Equal[D]): Boolean = {
      implicit val C = self
      val d1 = ((Cokleisli(f) =>= Cokleisli(g)) =>= Cokleisli(h)) run fa
      val d2 = (Cokleisli(f) =>= (Cokleisli(g) =>= Cokleisli(h))) run fa
      F.equal(d1, d2)
    }
  }

  def cobindLaw = new CobindLaws {}

  ////
  val cobindSyntax = new scalaz.syntax.CobindSyntax[F] { def F = Cobind.this }
}

object Cobind {
  @inline def apply[F[_]](implicit F: Cobind[F]): Cobind[F] = F

  import Isomorphism._

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: Cobind[G]): Cobind[F] =
    new IsomorphismCobind[F, G] {
      override def G: Cobind[G] = E
      override def iso: F <~> G = D
    }

  ////

  ////
}

trait IsomorphismCobind[F[_], G[_]] extends Cobind[F] with IsomorphismFunctor[F, G]{
  implicit def G: Cobind[G]
  ////

  override def cobind[A, B](fa: F[A])(f: F[A] => B): F[B] =
    iso.from(G.cobind(iso.to(fa))(f.compose(iso.from.apply)))

  override def cojoin[A](a: F[A]): F[F[A]] =
    iso.from(G.map(G.cojoin(iso.to(a)))(iso.from.apply))
  ////
}
