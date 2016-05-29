package scalaz

import scala.annotation.tailrec

sealed abstract class CorecursiveList[A] {
  type S
  val init: S
  val step: S => Maybe[(S, A)]
}

object CorecursiveList extends CorecursiveListInstances {
  private[this] final case class CorecursiveListImpl[S0, A](
    init: S0, step: S0 => Maybe[(S0, A)]) extends CorecursiveList[A] {
    type S = S0
  }

  def apply[S, A](init: S)(step: S => Maybe[(S, A)]): CorecursiveList[A] =
    CorecursiveListImpl(init, step)

  implicit val covariantInstance: Functor[CorecursiveList] with Foldable[CorecursiveList] with PlusEmpty[CorecursiveList] with Zip[CorecursiveList] =
    new Functor[CorecursiveList] with Foldable.FromFoldr[CorecursiveList]
        with PlusEmpty[CorecursiveList] with Zip[CorecursiveList] {
      override def map[A, B](fa: CorecursiveList[A])(f: A => B) =
        CorecursiveList(fa.init)(fa.step andThen (_ map {
          case (s, a) => (s, f(a))
        }))

      override def foldRight[A, B](fa: CorecursiveList[A], z: => B)(f: (A, => B) => B) = {
        def rec(s: fa.S): B =
          fa.step(s) cata ({case (s, a) => f(a, rec(s))}, z)
        rec(fa.init)
      }

      override def foldLeft[A, B](fa: CorecursiveList[A], z: B)(f: (B, A) => B) = {
        @tailrec def rec(z: B, s: fa.S): B = fa.step(s) match {
          case Maybe.Empty() => z
          case Maybe.Just((s, a)) => rec(f(z, a), s)
        }
        rec(z, fa.init)
      }

      override def empty[A] =
        CorecursiveList(())(Function const Maybe.Empty())

      override def plus[A](la: CorecursiveList[A], ra: => CorecursiveList[A]) = {
        val ra1 = Need(ra)
        type SS = la.S \/ ra1.value.S
        CorecursiveList(-\/(la.init): SS){
          case -\/(ls) =>
            (la.step(ls) map {case (ls, a) => (-\/(ls): SS, a)}
              orElse (ra1.value.step(ra1.value.init)
                        map {case (rs, a) => (\/-(rs), a)}))
          case \/-(rs) =>
            ra1.value.step(rs) map {case (rs, a) => (\/-(rs), a)}
        }
      }

      override def zip[A, B](l: => CorecursiveList[A], r: => CorecursiveList[B]) = {
        val l1 = l
        val r1 = r
        CorecursiveList((l1.init, r1.init)){
          case (ls, rs) =>
            Apply[Maybe].apply2(l1.step(ls), r1.step(rs)){
              case ((ls, la), (rs, ra)) => ((ls, rs), (la, ra))
            }
        }
      }
    }

  implicit def monoidInstance[A]: Monoid[CorecursiveList[A]] =
    covariantInstance.monoid

  implicit def orderInstance[A: Order]: Order[CorecursiveList[A]] =
    new Order[CorecursiveList[A]] with CorecursiveListEqual[A] {
      val A = Order[A]
      def order(l: CorecursiveList[A], r: CorecursiveList[A]) = {
        @tailrec def rec(ls: l.S, rs: r.S): Ordering =
          (l.step(ls), r.step(rs)) match {
            case (Maybe.Empty(), right) => right match {
              case Maybe.Empty() => Ordering.EQ
              case _ => Ordering.LT
            }
            case (Maybe.Just((ls2, la)), Maybe.Just((rs2, ra))) =>
              A.order(la, ra) match {
                case Ordering.EQ => rec(ls2, rs2)
                case o => o
              }
            case _ => Ordering.GT
          }
        rec(l.init, r.init)
      }
    }
}

sealed abstract class CorecursiveListInstances {
  implicit def equalInstance[A: Equal]: Equal[CorecursiveList[A]] =
    new CorecursiveListEqual[A] {
      val A = Equal[A]
    }
}

private sealed trait CorecursiveListEqual[A] extends Equal[CorecursiveList[A]] {
  val A: Equal[A]

  override def equal(l: CorecursiveList[A], r: CorecursiveList[A]): Boolean = {
    @tailrec def rec(ls: l.S, rs: r.S): Boolean =
      (l.step(ls), r.step(rs)) match {
        case (Maybe.Empty(), Maybe.Empty()) => true
        case (Maybe.Just((ls2, la)), Maybe.Just((rs2, ra))) =>
          A.equal(la, ra) && rec(ls2, rs2)
        case _ => false
      }
    rec(l.init, r.init)
  }
}
