package scalaz.tc

/**
  * In abstract algebra (and order theory), a meet-semilattice is a partially ordered set S together with
  * with an operation "meet" which computes the infimum (greatest lower bound), for every pair of finite,
  * nonempty subsets of S.
  *
  * The "meet" operation must satisfy the following laws:
  *
  *   Associativity:
  *     x `meet` (y `meet` z) == (x `meet` y) `meet` z
  *
  *   Commutativity:
  *     x `meet` y = y `meet` x
  *
  *   Idempotency (eye-dem-potent-see):
  *     x `meet` x = x
  *
  * As algebras, lattices form their own category, and therefore also have an associated homomorphism - an
  * order-homomorphism, which must preserve lower bounds. I.e. if h : M -> M' is to be a homomorphism of
  * meet-semilattices, then it must have the following properties:
  *
  * h (x `meet` y) = h(x) `meet` h(y)
  *
  * One can also view a meet-semilattice as a commutative Band.
  */
trait MeetSemiLatticeClass[A] extends BandClass[A] {
  def meet(a1: A, a2: => A): A =
    mappend(a1, a2)
}

