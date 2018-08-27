package scalaz.tc

import scalaz.data.{ Const, Identity }
import scala.{ Boolean, Unit }

/**
 * Lattices are an algebraic structures consisting of a set L together with two operations
 * "join" and "meet" that compute a unique supremum (least upper bound) and a unique infimum
 * greatest lower bound) for every pair of finite, non-empty subsets of L
 *
 * Along with the laws for [[JoinSemiLatticeClass]] and [[MeetSemiLatticeClass]], Lattices must
 * satisfy the following law:
 *
 * Absorption:
 *  a `join` (a `meet` b) == a `meet` (a `join` b) == a
 *
 * @tparam A
 */
trait LatticeClass[A] extends JoinSemiLatticeClass[A] with MeetSemiLatticeClass[A]

object LatticeClass {

  implicit val bool: LatticeClass[Boolean] = instanceOf(new LatticeClass[Boolean] {
    override def join(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def meet(a1: Boolean, a2: Boolean): Boolean = a1 && a2
  })

  implicit val unit: LatticeClass[Unit] = instanceOf(new LatticeClass[Unit] {
    override def join(a1: Unit, a2: Unit): Unit = ()
    override def meet(a1: Unit, a2: Unit): Unit = ()
  })

  implicit def identity[A](implicit L: Lattice[A]): LatticeClass[Identity[A]] =
    instanceOf(new LatticeClass[Identity[A]] {
      override def join(a1: Identity[A], a2: Identity[A]): Identity[A] =
        Identity(L.join(Identity.run(a1), Identity.run(a2)))
      override def meet(a1: Identity[A], a2: Identity[A]): Identity[A] =
        Identity(L.meet(Identity.run(a1), Identity.run(a2)))
    })

  implicit def const[A, B](implicit L: Lattice[A]): LatticeClass[Const[A, B]] =
    instanceOf(new LatticeClass[Const[A, B]] {
      override def join(a1: Const[A, B], a2: Const[A, B]): Const[A, B] = Const(L.join(Const.run(a1), Const.run(a2)))
      override def meet(a1: Const[A, B], a2: Const[A, B]): Const[A, B] = Const(L.meet(Const.run(a1), Const.run(a2)))
    })

  implicit def writer[A, B](implicit L: Lattice[B]): LatticeClass[A => B] =
    instanceOf(new LatticeClass[A => B] {
      override def join(a1: A => B, a2: A => B): A => B =
        a => L.join(a1(a), a2(a))
      override def meet(a1: A => B, a2: A => B): A => B =
        a => L.meet(a1(a), a2(a))
    })

  implicit def pair[A, B](implicit L1: Lattice[A], L2: Lattice[B]): LatticeClass[(A, B)] =
    instanceOf(new LatticeClass[(A, B)] {
      override def join(a1: (A, B), a2: (A, B)): (A, B) =
        (L1.join(a1._1, a2._1), L2.join(a1._2, a2._2))
      override def meet(a1: (A, B), a2: (A, B)): (A, B) =
        (L1.meet(a1._1, a2._1), L2.meet(a1._2, a2._2))
    })

}

trait LatticeSyntax {

  implicit final class ToLatticeOps[A](a: A) {

    def joinOrd(b: A)(implicit E: Eq[A], L: Lattice[A]): Boolean =
      E.equal(L.join(a, b), b)

    def meetOrd(b: A)(implicit E: Eq[A], L: Lattice[A]): Boolean =
      E.equal(L.meet(a, b), a)
  }
}
