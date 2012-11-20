package scalaz
package syntax

trait RingOps[F] extends Ops[F] {
  implicit def F: Ring[F]
  ////

  final def |*|(other: ⇒ F)      = F.multiply(self, other)
  final def multiply(other: ⇒ F) = F.multiply(self, other)
  ////

}

trait ToRingOps extends ToGroupOps {
  implicit def ToRingOps[F](v: F)(implicit F0: Ring[F]) =
    new RingOps[F] {
      def self = v
      implicit def F: Ring[F] = F0
      ////

      ////

    }
}

trait RingSyntax[F] extends GroupSyntax[F] {
  implicit def ToRingOps(v: F): RingOps[F] = new RingOps[F] {
    def self = v
    implicit def F: Ring[F] = RingSyntax.this.F
  }
  def F: Ring[F]
  ////

  ////

}
