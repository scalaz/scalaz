package scalaz

////
/**
 * [[scalaz.Band]] which is also commutative, i.e.
 * A + B == B + A
 * @see [[scalaz.SemiLattice.SemiLatticeLaw]]
 */
////
trait SemiLattice[F] extends Band[F] { self =>
  ////


  trait SemiLatticeLaw extends BandLaw {
    def commutative(a: F, b: F)(implicit F: Equal[F]) =
      F.equal(append(a, b), append(b, a))
  }

  def semiLatticeLaw = new SemiLatticeLaw {}
  
  ////
  val semiLatticeSyntax = new scalaz.syntax.SemiLatticeSyntax[F] { def F = SemiLattice.this }
}

object SemiLattice {
  @inline def apply[F](implicit F: SemiLattice[F]): SemiLattice[F] = F

  ////

  ////
}
