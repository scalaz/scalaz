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

  ////

  ////
}
