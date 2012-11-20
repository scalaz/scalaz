package scalaz

////
/**
 * A ring extends [[scalaz.Group]] with a multiply operation and an identity element for it (unit ring).
 *
 * References:
 * - [[http://en.wikipedia.org/wiki/Ring_(mathematics) Ring on Wikipedia]]
 * - [[http://mathworld.wolfram.com/Ring.html]]
 *
 * @see [[scalaz.syntax.RingOps]]
 * @see [[scalaz.Ring.RingLaw]]
 */
////

trait Ring[F] extends Group[F] { self ⇒
  ////
  /** The identity element for `multiply`. */
  def one: F

  def multiply(f1: F, f2: ⇒ F): F

  trait RingLaw extends GroupLaw {
    def commutative(f1: F, f2: F)(implicit F: Equal[F]): Boolean =
      F.equal(append(f1, f2), append(f2, f1))
    def multiplicationAssociative(f1: F, f2: F, f3: F)(implicit F: Equal[F]): Boolean =
      F.equal(multiply(multiply(f1, f2), f3), multiply(f1, multiply(f2, f3)))
    def leftDistributive(f1: F, f2: F, f3: F)(implicit F: Equal[F]): Boolean =
      F.equal(multiply(f1, append(f2, f3)), append(multiply(f1, f2), multiply(f1, f3)))
    def rightDistributive(f1: F, f2: F, f3: F)(implicit F: Equal[F]): Boolean =
      F.equal(multiply(append(f2, f3), f1), append(multiply(f2, f1), multiply(f3, f1)))
    def multiplicativeLeftIdentity(a: F)(implicit F: Equal[F]) =
      F.equal(a, multiply(one, a))
    def multiplicativeRightIdentity(a: F)(implicit F: Equal[F]) =
      F.equal(a, multiply(a, one))
  }
  def ringLaw = new RingLaw {}

  ////

  val ringSyntax = new scalaz.syntax.RingSyntax[F] { def F = Ring.this }
}

object Ring {
  @inline def apply[F](implicit F: Ring[F]): Ring[F] = F

  ////

  ////

}
