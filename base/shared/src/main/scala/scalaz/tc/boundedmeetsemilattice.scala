package scalaz.tc

import scalaz.data.{ Const, Identity }
import scala.{ Boolean, Unit }

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

object BoundedMeetSemiLatticeClass {
  implicit val bool: BoundedMeetSemiLatticeClass[Boolean] = instanceOf(new BoundedMeetSemiLatticeClass[Boolean] {
    override def top: Boolean                            = true
    override def meet(a1: Boolean, a2: Boolean): Boolean = a1 && a2
  })

  implicit val unit: BoundedMeetSemiLatticeClass[Unit] = instanceOf(new BoundedMeetSemiLatticeClass[Unit] {
    override def top: Unit                      = ()
    override def meet(a1: Unit, a2: Unit): Unit = ()
  })

  implicit def identity[A](implicit L: BoundedMeetSemiLattice[A]): BoundedMeetSemiLatticeClass[Identity[A]] =
    instanceOf(new BoundedMeetSemiLatticeClass[Identity[A]] {
      override def top: Identity[A] = Identity(L.top)
      override def meet(a1: Identity[A], a2: Identity[A]): Identity[A] =
        Identity(L.meet(Identity.run(a1), Identity.run(a2)))
    })

  implicit def const[A, B](implicit L: BoundedMeetSemiLattice[A]): BoundedMeetSemiLatticeClass[Const[A, B]] =
    instanceOf(new BoundedMeetSemiLatticeClass[Const[A, B]] {
      override def top: Const[A, B]                                    = Const(L.top)
      override def meet(a1: Const[A, B], a2: Const[A, B]): Const[A, B] = Const(L.meet(Const.run(a1), Const.run(a2)))
    })

  implicit def writer[A, B](implicit L: BoundedMeetSemiLattice[B]): BoundedMeetSemiLatticeClass[A => B] =
    instanceOf(new BoundedMeetSemiLatticeClass[A => B] {
      override def top: A => B = _ => L.top
      override def meet(a1: A => B, a2: A => B): A => B =
        a => L.meet(a1(a), a2(a))
    })

  implicit def pair[A, B](implicit L1: BoundedMeetSemiLattice[A],
                          L2: BoundedMeetSemiLattice[B]): BoundedMeetSemiLatticeClass[(A, B)] =
    instanceOf(new BoundedMeetSemiLatticeClass[(A, B)] {
      override def top: (A, B) = (L1.top, L2.top)
      override def meet(a1: (A, B), a2: (A, B)): (A, B) =
        (L1.meet(a1._1, a2._1), L2.meet(a1._2, a2._2))
    })
}
