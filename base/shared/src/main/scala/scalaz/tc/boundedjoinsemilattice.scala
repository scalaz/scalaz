package scalaz.tc

import scalaz.data.{ Const, Identity }

import scala.{ Boolean, Unit }

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

object BoundedJoinSemiLatticeClass {

  implicit val bool: BoundedJoinSemiLatticeClass[Boolean] = instanceOf(new BoundedJoinSemiLatticeClass[Boolean] {
    override def bottom: Boolean                         = false
    override def join(a1: Boolean, a2: Boolean): Boolean = a1 || a2
  })

  implicit val unit: BoundedJoinSemiLatticeClass[Unit] = instanceOf(new BoundedJoinSemiLatticeClass[Unit] {
    override def bottom: Unit                   = ()
    override def join(a1: Unit, a2: Unit): Unit = ()
  })

  implicit def identity[A](implicit L: BoundedJoinSemiLattice[A]): BoundedJoinSemiLatticeClass[Identity[A]] =
    instanceOf(new BoundedJoinSemiLatticeClass[Identity[A]] {
      override def bottom: Identity[A] = Identity(L.bottom)
      override def join(a1: Identity[A], a2: Identity[A]): Identity[A] =
        Identity(L.join(Identity.run(a1), Identity.run(a2)))
    })

  implicit def const[A, B](implicit L: BoundedJoinSemiLattice[A]): BoundedJoinSemiLatticeClass[Const[A, B]] =
    instanceOf(new BoundedJoinSemiLatticeClass[Const[A, B]] {
      override def bottom: Const[A, B]                                 = Const(L.bottom)
      override def join(a1: Const[A, B], a2: Const[A, B]): Const[A, B] = Const(L.join(Const.run(a1), Const.run(a2)))
    })

  implicit def writer[A, B](implicit L: BoundedJoinSemiLattice[B]): BoundedJoinSemiLatticeClass[A => B] =
    instanceOf(new BoundedJoinSemiLatticeClass[A => B] {
      override def bottom: A => B = _ => L.bottom
      override def join(a1: A => B, a2: A => B): A => B =
        a => L.join(a1(a), a2(a))
    })

  implicit def pair[A, B](implicit L1: BoundedJoinSemiLattice[A],
                          L2: BoundedJoinSemiLattice[B]): BoundedJoinSemiLatticeClass[(A, B)] =
    instanceOf(new BoundedJoinSemiLatticeClass[(A, B)] {
      override def bottom: (A, B) = (L1.bottom, L2.bottom)
      override def join(a1: (A, B), a2: (A, B)): (A, B) =
        (L1.join(a1._1, a2._1), L2.join(a1._2, a2._2))
    })
}
