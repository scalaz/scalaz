package scalaz.tc

/**
  * Lattices are an algebraic structures consisting of a set L together with two operations
  * "join" and "meet" that compute a unique supremum (least upper bound) and a unique infimum
  * greatest lower bound) for every pair of finite, non-empty subsets of L
  *
  * See [[JoinSemiLatticeClass]] and [[MeetSemiLatticeClass]] for laws.
  *
  * @tparam A
  */
trait LatticeClass[A] extends JoinSemiLatticeClass[A] with MeetSemiLatticeClass[A]
