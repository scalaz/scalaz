package scalaz

////
/**
 *
 */
////
trait Pointed[F[_]] extends Functor[F] { self =>
  ////

  def point[A](a: => A): F[A]

  // derived functions

  /** alias for `point` */
  def pure[A](a: => A): F[A] = point(a)

  /**The composition of Pointeds `F` and `G`, `[x]F[G[x]]`, is a Pointed */
  def compose[G[_]](implicit G0: Pointed[G]): Pointed[({type λ[α] = F[G[α]]})#λ] = new CompositionPointed[F, G] {
    implicit def F = self

    implicit def G = G0
  }

  /**The product of Pointeds `F` and `G`, `[x](F[x], G[x]])`, is a Pointed */
  def product[G[_]](implicit G0: Pointed[G]): Pointed[({type λ[α] = (F[α], G[α])})#λ] = new ProductPointed[F, G] {
    implicit def F = self

    implicit def G = G0
  }

  ////
  val pointedSyntax = new scalaz.syntax.PointedSyntax[F] {}
}

object Pointed {
  @inline def apply[F[_]](implicit F: Pointed[F]): Pointed[F] = F

  ////

  ////
}

