package scalaz

////
/**
 * [[scalaz.Semigroup]] which is also idempotent, i.e. appending a value with
 * itself results in the same value.
 *
 * @see [[scalaz.Band.BandLaw]]
 */
////
trait Band[F] extends Semigroup[F] { self =>
  ////

  // derived functions

  /**
    * The default definition exploits idempotency to optimise to `O(1)`
    */
  override def multiply1(value: F, n: Int): F =
    value

  /**
    * Band instances must satisfy [[scalaz.Semigroup.SemigroupLaw]] and 1 additional law:
    *
    *  - '''idempotency''': `forall a. append(a, a) == a`
    */
  trait BandLaw extends SemigroupLaw {
    def idempotency(a: F)(implicit F: Equal[F]) = F.equal(a, append(a, a))
  }
  def bandLaw = new BandLaw {}

  ////
  val bandSyntax: scalaz.syntax.BandSyntax[F] =
    new scalaz.syntax.BandSyntax[F] { def F = Band.this }
}

object Band {
  @inline def apply[F](implicit F: Band[F]): Band[F] = F

  import Isomorphism._

  def fromIso[F, G](D: F <=> G)(implicit M: Band[G]): Band[F] =
    new IsomorphismBand[F, G] {
      override def G: Band[G] = M
      override def iso: F <=> G = D
    }

  ////

  ////
}
