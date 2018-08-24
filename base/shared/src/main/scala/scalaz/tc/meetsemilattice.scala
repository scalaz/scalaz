package scalaz.tc

import scalaz.data.{ Const, Endo, Identity }

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

object MeetSemiLatticeClass extends MeetSemiLatticeInstances

trait MeetSemiLatticeInstances {

  implicit val bool: MeetSemiLatticeClass[Boolean] =
    (a1, a2) => a1 && a2

  implicit val unit: MeetSemiLatticeClass[Unit] =
    (_, _) => ()

  implicit def identity[A](implicit L: MeetSemiLattice[A]): MeetSemiLatticeClass[Identity[A]] =
    (id1, id2) => Identity(L.meet(Identity.run(id1), Identity.run(id2)))

  implicit def writer[A, B](implicit L: MeetSemiLatticeClass[B]): MeetSemiLatticeClass[A => B] =
    (f1, f2) => a => L.meet(f1(a), f2(a))

  implicit def pair[A, B](
    implicit L: MeetSemiLatticeClass[A],
    M: MeetSemiLatticeClass[B]
  ): MeetSemiLatticeClass[(A, B)] =
    (a, b) => (L.meet(a._1, b._1), M.meet(a._2, b._2))

  implicit def const[A, B](implicit L: MeetSemiLatticeClass[A]): MeetSemiLatticeClass[Const[A, B]] =
    (c, d) => Const(L.meet(Const.run(c), Const.run(d)))
}
