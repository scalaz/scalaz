package scalaz

////
/**
 *
 */
////
trait Cojoin[F[_]] extends Functor[F] { self =>
  ////
  /** Also known as `duplicate` */
  def cojoin[A](a: F[A]): F[F[A]]

  // derived functions

  def extend[A, B](a: F[A])(f: F[A] => B): F[B] =
    map(cojoin(a))(f)

  trait CojoinLaws {
    def cobindAssociative[A, B, C, D](fa: F[A], f: F[A] => B, g: F[B] => C, h: F[C] => D)(implicit F: Equal[D]): Boolean = {
      implicit val C = self
      val d1 = ((Cokleisli(f) =>= Cokleisli(g)) =>= Cokleisli(h)) run fa
      val d2 = (Cokleisli(f) =>= (Cokleisli(g) =>= Cokleisli(h))) run fa
      F.equal(d1, d2)
    }
  }

  def cojoinLaw = new CojoinLaws {}

  ////
  val cojoinSyntax = new scalaz.syntax.CojoinSyntax[F] { def F = Cojoin.this }
}

object Cojoin {
  @inline def apply[F[_]](implicit F: Cojoin[F]): Cojoin[F] = F

  ////
  /** Define `Cojoin` in terms of `Cobind` */
  trait FromCobind[F[_]] extends Cojoin[F] {
    self: Cobind[F] =>

    def cojoin[A](fa: F[A]): F[F[A]] = cobind(fa)(fa => fa)

    override def extend[A, B](a: F[A])(f: F[A] => B): F[B] = cobind(a)(f)
  }

  ////
}
