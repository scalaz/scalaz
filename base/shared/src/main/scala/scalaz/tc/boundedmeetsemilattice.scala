package scalaz.tc

/**
 * A Meet Semi-Lattice with a top element ⊤ such that the following laws hold:
 *
 * Identity:
 *   x `meet` ⊤ == ⊤ `meet` x = x
 *
 * One may view Bounded semi-lattices as idempotent commutative Monoids
 *
 * @tparam A
 */
trait BoundedMeetSemiLatticeClass[A] extends MeetSemiLatticeClass[A] {
  def top: A
}
