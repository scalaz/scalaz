package scalaz

////
/**
 *
 */
////
trait Align[F[_]] extends Functor[F] { self =>
  ////

  import \&/._

  def alignWith[A, B, C](f: A \&/ B => C): (F[A], F[B]) => F[C]

  def product[G[_]](implicit G0: Align[G]): Align[λ[α => (F[α], G[α])]] = new ProductAlign[F, G] {
    override def F = self

    override def G = G0
  }

  def align[A, B](a: F[A], b: F[B]): F[A \&/ B] =
    alignWith[A, B, A \&/ B](identity)(a, b)

  def padWith[A, B, C](f: (Option[A], Option[B]) => C): (F[A], F[B]) => F[C] =
    alignWith(t => {
      val (a, b) = t.pad
      f(a, b)
    })

  def pad[A, B]: (F[A], F[B]) => F[(Option[A], Option[B])] =
    padWith((a, b) => (a, b))

  def merge[A](a1: F[A], a2: F[A])(implicit S: Semigroup[A]): F[A] =
    alignWith[A, A, A](t => \&/.merge(t))(a1, a2)

  def alignSwap[A, B](a: F[A], b: F[B]): F[B \&/ A] =
    alignWith[A, B, B \&/ A](_.swap)(a, b)

  def alignA[A, B](a: F[A], b: F[B]): F[Option[A]] =
    alignWith[A, B, Option[A]](_.a)(a, b)

  def alignB[A, B](a: F[A], b: F[B]): F[Option[B]] =
    alignWith[A, B, Option[B]](_.b)(a, b)

  def alignThis[A, B](a: F[A], b: F[B]): F[Option[A]] =
    alignWith[A, B, Option[A]](_.onlyThis)(a, b)

  def alignThat[A, B](a: F[A], b: F[B]): F[Option[B]] =
    alignWith[A, B, Option[B]](_.onlyThat)(a, b)

  def alignBoth[A, B](a: F[A], b: F[B]): F[Option[(A, B)]] =
    alignWith[A, B, Option[(A, B)]](_.onlyBoth)(a, b)

  trait AlignLaw extends FunctorLaw {
    def collapse[A](a: F[A])(implicit E: Equal[F[A \&/ A]]): Boolean =
      E.equal(map(a)(x => Both(x, x): A \&/ A), align(a, a))
  }

  def alignLaw = new AlignLaw {}

  ////
  val alignSyntax: scalaz.syntax.AlignSyntax[F] =
    new scalaz.syntax.AlignSyntax[F] { def F = Align.this }
}

object Align {
  @inline def apply[F[_]](implicit F: Align[F]): Align[F] = F

  import Isomorphism._

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: Align[G]): Align[F] =
    new IsomorphismAlign[F, G] {
      override def G: Align[G] = E
      override def iso: F <~> G = D
    }

  ////
  implicit def idInstance: Align[Id.Id] = Id.id

  ////
}
