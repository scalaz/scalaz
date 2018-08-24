package scalaz.tc

import Predef._
import scalaz.data.{ Const, Identity }

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

object JoinSemiLatticeClass extends JoinSemiLatticeInstances

trait JoinSemiLatticeInstances {

  implicit val bool: JoinSemiLatticeClass[Boolean] =
    (a1, a2) => a1 || a2

  implicit val unit: JoinSemiLatticeClass[Unit] =
    (_, _) => ()

  implicit def identity[A](implicit L: JoinSemiLattice[A]): JoinSemiLatticeClass[Identity[A]] =
    (id1, id2) => Identity(L.join(Identity.run(id1), Identity.run(id2)))

  implicit def writer[A, B](implicit L: JoinSemiLatticeClass[B]): JoinSemiLatticeClass[A => B] =
    (f1, f2) => a => L.join(f1(a), f2(a))

  implicit def pair[A, B](
    implicit L: JoinSemiLatticeClass[A],
    M: JoinSemiLatticeClass[B]
  ): JoinSemiLatticeClass[(A, B)] =
    (a, b) => (L.join(a._1, b._1), M.join(a._2, b._2))

  implicit def const[A, B](implicit L: JoinSemiLatticeClass[A]): JoinSemiLatticeClass[Const[A, B]] =
    (c, d) => Const(L.join(Const.run(c), Const.run(d)))
}
