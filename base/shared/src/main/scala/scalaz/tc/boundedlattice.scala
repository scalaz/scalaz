package scalaz.tc

import scalaz.data.{ Const, Identity }

import scala.{ Boolean, Unit }

/**
 * A lattice with both upper and lower bounds
 *
 * @tparam A
 */
trait BoundedLatticeClass[A]
    extends LatticeClass[A]
    with BoundedJoinSemiLatticeClass[A]
    with BoundedMeetSemiLatticeClass[A]

object BoundedLatticeClass {
  implicit val bool: BoundedLatticeClass[Boolean] = instanceOf(new BoundedLatticeClass[Boolean] {
    override def top: Boolean                            = true
    override def bottom: Boolean                         = false
    override def join(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def meet(a1: Boolean, a2: Boolean): Boolean = a1 && a2
  })

  implicit val unit: BoundedLatticeClass[Unit] = instanceOf(new BoundedLatticeClass[Unit] {
    override def top: Unit                      = ()
    override def bottom: Unit                   = ()
    override def join(a1: Unit, a2: Unit): Unit = ()
    override def meet(a1: Unit, a2: Unit): Unit = ()
  })

  implicit def identity[A](implicit L: BoundedLattice[A]): BoundedLatticeClass[Identity[A]] =
    instanceOf(new BoundedLatticeClass[Identity[A]] {
      override def bottom: Identity[A] = Identity(L.bottom)
      override def top: Identity[A]    = Identity(L.top)
      override def join(a1: Identity[A], a2: Identity[A]): Identity[A] =
        Identity(L.join(Identity.run(a1), Identity.run(a2)))
      override def meet(a1: Identity[A], a2: Identity[A]): Identity[A] =
        Identity(L.meet(Identity.run(a1), Identity.run(a2)))
    })

  implicit def const[A, B](implicit L: BoundedLattice[A]): BoundedLatticeClass[Const[A, B]] =
    instanceOf(new BoundedLatticeClass[Const[A, B]] {
      override def bottom: Const[A, B]                                 = Const(L.bottom)
      override def top: Const[A, B]                                    = Const(L.top)
      override def join(a1: Const[A, B], a2: Const[A, B]): Const[A, B] = Const(L.join(Const.run(a1), Const.run(a2)))
      override def meet(a1: Const[A, B], a2: Const[A, B]): Const[A, B] = Const(L.meet(Const.run(a1), Const.run(a2)))
    })

  implicit def writer[A, B](implicit L: BoundedLattice[B]): BoundedLatticeClass[A => B] =
    instanceOf(new BoundedLatticeClass[A => B] {
      override def bottom: A => B = _ => L.bottom
      override def top: A => B    = _ => L.top
      override def join(a1: A => B, a2: A => B): A => B =
        a => L.join(a1(a), a2(a))
      override def meet(a1: A => B, a2: A => B): A => B =
        a => L.meet(a1(a), a2(a))
    })

  implicit def pair[A, B](implicit L1: BoundedLattice[A], L2: BoundedLattice[B]): BoundedLatticeClass[(A, B)] =
    instanceOf(new BoundedLatticeClass[(A, B)] {
      override def bottom: (A, B) = (L1.bottom, L2.bottom)
      override def top: (A, B)    = (L1.top, L2.top)
      override def join(a1: (A, B), a2: (A, B)): (A, B) =
        (L1.join(a1._1, a2._1), L2.join(a1._2, a2._2))
      override def meet(a1: (A, B), a2: (A, B)): (A, B) =
        (L1.meet(a1._1, a2._1), L2.meet(a1._2, a2._2))
    })
}

trait BoundedLatticeSyntax {

  implicit final class ToBoundedlatticeOpsA[A](a: A) {
    def fromBool(b: Boolean)(implicit L: BoundedLattice[A]): A =
      if (b) L.top else L.bottom
  }

  implicit final class ToBoundedLatticeOpsFA[F[_], A](fa: F[A]) {
    def joins(implicit F: Foldable[F], L: BoundedJoinSemiLattice[A]): A =
      F.foldLeft(fa, L.bottom)(L.join)
    def meets(implicit F: Foldable[F], L: BoundedMeetSemiLattice[A]): A =
      F.foldLeft(fa, L.top)(L.meet)
  }
}
