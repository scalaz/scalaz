package scalaz

import scala.annotation.tailrec
import Maybe.{Empty, Just, just}

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

  def fromStream[A](s: Stream[A]): CorecursiveList[A] =
    CorecursiveList(s){
      case Stream.Empty => Empty()
      case x #:: xs => just((xs, x))
    }

  implicit val covariantInstance: MonadPlus[CorecursiveList] with Foldable[CorecursiveList] with Zip[CorecursiveList] =
    new MonadPlus[CorecursiveList] with Foldable.FromFoldr[CorecursiveList]
        with Zip[CorecursiveList] {
      override def map[A, B](fa: CorecursiveList[A])(f: A => B) =
        CorecursiveList(fa.init)(fa.step andThen (_ map {
          case (s, a) => (s, f(a))
        }))

      override def ap[A, B](fa0: => CorecursiveList[A])(ff0: => CorecursiveList[A => B]) = {
        val fa = fa0
        val ff = ff0
        def bstep(sa: fa.S, ffs: Maybe[(ff.S, A => B)])
            : Maybe[((fa.S, Maybe[(ff.S, A => B)]), B)] =
          ffs.flatMap{case (sf, f) =>
            (fa.step(sa) map {case (sa, a) => ((sa, ffs), f(a))}
              orElse bstep(fa.init, ff.step(sf)))
          }
        CorecursiveList((fa.init, ff.step(ff.init)))((bstep _).tupled)
      }

      override def bind[A, B](fa: CorecursiveList[A])(f: A => CorecursiveList[B]) = {
        def bstep(sa: fa.S, mfb: Maybe[CorecursiveList[B]])
            : Maybe[((fa.S, Maybe[CorecursiveList[B]]), B)] = mfb match {
          case Empty() => fa.step(sa) flatMap {case (sa, a) =>
            bstep(sa, just(f(a)))
          }
          case Just(fb) => fb.step(fb.init) map {
            case (sb, b) => ((sa, just(CorecursiveList(sb)(fb.step))), b)
          } orElse bstep(sa, Empty())
        }
        CorecursiveList((fa.init, Empty(): Maybe[CorecursiveList[B]])){
          case (sa, mfb) => bstep(sa, mfb)
        }
      }

      override def filter[A](fa: CorecursiveList[A])(f: A => Boolean) = {
        @tailrec def step(s: fa.S): Maybe[(fa.S, A)] = fa.step(s) match {
          case Empty() => Empty()
          case j@Just((s, a)) =>
            if (f(a)) j else step(s)
        }
        CorecursiveList(fa.init)(step(_))
      }

      override def point[A](a: => A) =
        CorecursiveList(false){b =>
          if (b) Empty() else just((true, a))
        }

      override def foldRight[A, B](fa: CorecursiveList[A], z: => B)(f: (A, => B) => B) = {
        def rec(s: fa.S): B =
          fa.step(s) cata ({case (s, a) => f(a, rec(s))}, z)
        rec(fa.init)
      }

      override def foldLeft[A, B](fa: CorecursiveList[A], z: B)(f: (B, A) => B) = {
        @tailrec def rec(z: B, s: fa.S): B = fa.step(s) match {
          case Empty() => z
          case Just((s, a)) => rec(f(z, a), s)
        }
        rec(z, fa.init)
      }

      override def empty[A] =
        CorecursiveList(())(Function const Empty())

      override def plus[A](la: CorecursiveList[A], ra0: => CorecursiveList[A]) = {
        val ra = Need(ra0)
        type SS = la.S \/ ra.value.S
        def rightStep(rs: ra.value.S) =
          ra.value.step(rs) map {case (rs, a) => (\/-(rs): SS, a)}
        CorecursiveList(-\/(la.init): SS){
          case -\/(ls) =>
            (la.step(ls) map {case (ls, a) => (-\/(ls): SS, a)}
              orElse rightStep(ra.value.init))
          case \/-(rs) =>
            rightStep(rs)
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
            case (Empty(), right) => right match {
              case Empty() => Ordering.EQ
              case _ => Ordering.LT
            }
            case (Just((ls2, la)), Just((rs2, ra))) =>
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
        case (Empty(), Empty()) => true
        case (Just((ls2, la)), Just((rs2, ra))) =>
          A.equal(la, ra) && rec(ls2, rs2)
        case _ => false
      }
    rec(l.init, r.init)
  }
}
