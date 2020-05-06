package scalaz

////
/** `Divisible` is the contravariant analogue of `scalaz.Applicative`
 *
 * @see [[https://github.com/ekmett/contravariant/blob/v1.3.2/src/Data/Functor/Contravariant/Divisible.hs]]
 * @see [[https://youtu.be/cB8DapKQz-I?t=20m35s ZuriHac 2015 - Discrimination is Wrong: Improving Productivity]]
 */
////
trait Divisible[F[_]] extends Divide[F] { self =>
  ////

  def conquer[A]: F[A]

  trait DivisibleLaw extends DivideLaw {
    def rightIdentity[A](fa: F[A])(implicit E: Equal[F[A]]): Boolean =
      E.equal(divide(fa, conquer[A])(delta), fa)

    def leftIdentity[A](fa: F[A])(implicit E: Equal[F[A]]): Boolean =
      E.equal(divide(conquer[A], fa)(delta), fa)
  }

  def divisibleLaw = new DivisibleLaw {}

  ////
  val divisibleSyntax: scalaz.syntax.DivisibleSyntax[F] =
    new scalaz.syntax.DivisibleSyntax[F] { def F = Divisible.this }
}

object Divisible {
  @inline def apply[F[_]](implicit F: Divisible[F]): Divisible[F] = F

  import Isomorphism._

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: Divisible[G]): Divisible[F] =
    new IsomorphismDivisible[F, G] {
      override def G: Divisible[G] = E
      override def iso: F <~> G = D
    }

  ////

  ////
}
