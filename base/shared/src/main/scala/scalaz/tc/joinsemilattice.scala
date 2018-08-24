package scalaz.tc

/**
  * In abstract algebra (and order theory), a join-semilattice is a partially ordered set S together with
  * with an operation "join" which computes the supremum (least upper bound) for every pair of finite, 
  * nonempty subsets of S.
  *
  * The "join" operation must satisfy the following laws:
  *
  *   Associativity:
  *     x `join` (y `join` z) == (x `join` y) `join` z
  *
  *   Commutativity:
  *     x `join` y = y `join` x
  *
  *   Idempotency (eye-dem-potent-see):
  *     x `join` x = x
  *
  * As algebras, lattices form their own category, and therefore also have an associated homomorphism - an
  * order-homomorphism, which must preserve lower bounds. I.e. if h : M -> M' is to be a homomorphism of
  * join-semilattices, then it must have the following properties:
  *
  * h (x `join` y) = h(x) `join` h(y)
  *
  * One can also view a join-semilattice as a commutative Band.
  */
trait JoinSemiLatticeClass[A] extends BandClass[A] {
  def join(a1: A, a2: => A): A =
    mappend(a1, a2)
}