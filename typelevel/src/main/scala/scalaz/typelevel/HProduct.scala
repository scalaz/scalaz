package scalaz
package typelevel

import Typelevel._

/**
 * `HList` version of [[scalaz.ProductFunctor]]
 */
trait HProductFunctor[F[_], G[_]] extends Functor[({type λ[α] = HCons[F[α], HCons[G[α], HNil]]})#λ] {
  implicit def F: Functor[F]

  implicit def G: Functor[G]

  override def map[A, B](fga: HCons[F[A], HCons[G[A], HNil]])(f: (A) => B) =
    F.map(fga.head)(f) :: G.map(fga.tail.head)(f) :: HNil
}

/**
 * `HList` version of [[scalaz.ProductPointed]]
 */
private[scalaz] trait HProductPointed[F[_], G[_]] extends Pointed[({type λ[α] = HCons[F[α], HCons[G[α], HNil]]})#λ] with HProductFunctor[F, G] {
  implicit def F: Pointed[F]

  implicit def G: Pointed[G]

  def point[A](a: => A) = F.point(a) :: G.point(a) :: HNil
}

/**
 * `HList` version of [[scalaz.ProductApplicative]]
 */
private[scalaz] trait HProductApplicative[F[_], G[_]] extends Applicative[({type λ[α] = HCons[F[α], HCons[G[α], HNil]]})#λ] with HProductPointed[F, G] {
  implicit def F: Applicative[F]

  implicit def G: Applicative[G]

  def ap[A, B](fga: => HCons[F[A], HCons[G[A], HNil]])(f: => HCons[F[(A) => B], HCons[G[(A) => B], HNil]]) = F.ap(fga.head)(f.head) :: G.ap(fga.tail.head)(f.tail.head) :: HNil
}
