package scalaz

////
/**
 * [[scalaz.Applicative]] combined with [[scalaz.PlusEmpty]].
 */
////
trait ApplicativePlus[F[_]] extends Applicative[F] with PlusEmpty[F] { self =>
  ////

  /**The composition of ApplicativePlus `F` and Applicative `G`, `[x]F[G[x]]`, is a ApplicativePlus */
  override def compose[G[_]](implicit G0: Applicative[G]): ApplicativePlus[λ[α => F[G[α]]]] =
    new CompositionApplicativePlus[F, G] {
      override def F = self
      override def G = G0
    }

  /**The product of ApplicativePlus `F` and `G`, `[x](F[x], G[x]])`, is a ApplicativePlus */
  def product[G[_]](implicit G0: ApplicativePlus[G]): ApplicativePlus[λ[α => (F[α], G[α])]] =
    new ProductApplicativePlus[F, G] {
      override def F = self
      override def G = G0
    }

  ////
  val applicativePlusSyntax: scalaz.syntax.ApplicativePlusSyntax[F] =
    new scalaz.syntax.ApplicativePlusSyntax[F] { def F = ApplicativePlus.this }
}

abstract class ApplicativePlusInstances {

  implicit def applicativePlusAlt[F[_]](implicit A0: ApplicativePlus[F]): Alt[F] = new ApplicativePlusAlt[F] {
    implicit def A: ApplicativePlus[F] = A0
  }
}

object ApplicativePlus extends ApplicativePlusInstances {
  @inline def apply[F[_]](implicit F: ApplicativePlus[F]): ApplicativePlus[F] = F

  import Isomorphism._

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: ApplicativePlus[G]): ApplicativePlus[F] =
    new IsomorphismApplicativePlus[F, G] {
      override def G: ApplicativePlus[G] = E
      override def iso: F <~> G = D
    }

  ////

  ////
}

trait IsomorphismApplicativePlus[F[_], G[_]] extends ApplicativePlus[F] with IsomorphismApplicative[F, G] with IsomorphismPlusEmpty[F, G]{
  implicit def G: ApplicativePlus[G]
  ////

  ////
}

private trait ApplicativePlusAlt[F[_]] extends Alt[F] {
  implicit def A: ApplicativePlus[F]

  def point[A](a: => A): F[A] = A.point(a)

  def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B] = A.ap(fa)(f)

  def alt[A](a1: =>F[A], a2: =>F[A]): F[A] = A.plus(a1, a2)
}
