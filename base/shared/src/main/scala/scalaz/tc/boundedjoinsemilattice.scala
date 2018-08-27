package scalaz.tc

/**
 * A Join Semi-lattice which has a unique bottom element, denoted ⊥ such that the
 * following laws hold:
 *
 * Identity:
 *   x `join` ⊥ == ⊥ `join` x == x
 *
 * One may view Bounded semi-lattices as idempotent commutative Monoids
 *
 * @tparam A
 */
trait BoundedJoinSemiLatticeClass[A] extends JoinSemiLatticeClass[A] {
  def bottom: A
}
