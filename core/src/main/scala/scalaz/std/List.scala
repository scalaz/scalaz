package scalaz
package std

import scalaz.Id._
import annotation.tailrec

trait ListInstances0 {
  implicit def listEqual[A](implicit A0: Equal[A]) = new ListEqual[A] {
    implicit def A = A0
  }
}

trait ListInstances extends ListInstances0 {
  implicit val listInstance = new Traverse[List] with MonadPlus[List] with Each[List] with Index[List] with Length[List] with Zip[List] with Unzip[List] with IsEmpty[List] with Cobind[List] {
    def each[A](fa: List[A])(f: A => Unit) = fa foreach f
    override def index[A](fa: List[A], i: Int) = fa.lift.apply(i)
    // TODO remove after removal of Index
    override def indexOr[A](fa: List[A], default: => A, i: Int) = super[Traverse].indexOr(fa, default, i)
    override def length[A](fa: List[A]) = fa.length
    def point[A](a: => A) = scala.List(a)
    def bind[A, B](fa: List[A])(f: A => List[B]) = fa flatMap f
    def empty[A] = scala.List()
    def plus[A](a: List[A], b: => List[A]) = a ++ b
    override def map[A, B](l: List[A])(f: A => B) = l map f

    def zip[A, B](a: => List[A], b: => List[B]) = a zip b
    def unzip[A, B](a: List[(A, B)]) = a.unzip

    def traverseImpl[F[_], A, B](l: List[A])(f: A => F[B])(implicit F: Applicative[F]) = {
      // implementation with `foldRight` leads to SOE in:
      //
      //  def wc(c: Char) = State[Boolean, Int]{(inWord) =>
      //    val s = c != ' '
      //    (test(!(inWord && s)), s)
      //  }
      //  val X = StateT.stateMonad[Boolean].traverse(List[Char]('a'))(wc)

      // foldRight(l, F.point(List[B]())) {
      //   (a, fbs) => F.map2(f(a), fbs)(_ :: _)
      // }

      DList.fromList(l).foldr(F.point(List[B]())) {
         (a, fbs) => F.apply2(f(a), fbs)(_ :: _)
      }
    }

    override def traverseS[S,A,B](l: List[A])(f: A => State[S,B]): State[S,List[B]] = {
      State((s: S) => {
        val buf = new collection.mutable.ListBuffer[B]
        var cur = s
        l.foreach { a => val bs = f(a)(cur); buf += bs._2; cur = bs._1 }
        (cur, buf.toList)
      })
    }

    override def foldLeft[A, B](fa: List[A], z: B)(f: (B, A) => B): B = fa.foldLeft(z)(f)

    override def foldRight[A, B](fa: List[A], z: => B)(f: (A, => B) => B) = {
      import scala.collection.mutable.ArrayStack
      val s = new ArrayStack[A]
      fa.foreach(a => s += a)
      var r = z
      while (!s.isEmpty) {
        // force and copy the value of r to ensure correctness
        val w = r
        r = f(s.pop, w)
      }
      r
    }

    override def toList[A](fa: List[A]) = fa

    def isEmpty[A](fa: List[A]) = fa.isEmpty

    def cobind[A, B](fa: List[A])(f: List[A] => B) =
      fa match {
        case Nil => Nil
        case _::t => f(fa) :: cobind(t)(f)
      }

    override def cojoin[A](a: List[A]) =
      a match {
        case Nil => Nil
        case _::t => a :: cojoin(t)
      }

  }

  implicit def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def append(f1: List[A], f2: => List[A]) = f1 ::: f2
    def zero: List[A] = Nil
  }

  implicit def listShow[A: Show]: Show[List[A]] = new Show[List[A]] {
    override def show(as: List[A]) = "[" +: Cord.mkCord(",", as.map(Show[A].show):_*) :+ "]"
  }

  implicit def listOrder[A](implicit A0: Order[A]): Order[List[A]] = new ListOrder[A] {
    implicit def A = A0
  }
}

trait ListFunctions {
  /** Intersperse the element `a` between each adjacent pair of elements in `as` */
  final def intersperse[A](as: List[A], a: A): List[A] = {
    @tailrec
    def intersperse0(accum: List[A], rest: List[A]): List[A] = rest match {
      case Nil      => accum
      case x :: Nil => x :: accum
      case h :: t   => intersperse0(a :: h :: accum, t)
    }
    intersperse0(Nil, as).reverse
  }

  /** [[scala.Nil]] with a sometimes more convenient type */
  final def nil[A]: List[A] = Nil

  final def toNel[A](as: List[A]): Option[NonEmptyList[A]] = as match {
    case Nil    => None
    case h :: t => Some(NonEmptyList.nel(h, t))
  }

  final def toZipper[A](as: List[A]): Option[Zipper[A]] =
    stream.toZipper(as.toStream)

  final def zipperEnd[A](as: List[A]): Option[Zipper[A]] =
    stream.zipperEnd(as.toStream)

  /**
   * Returns `f` applied to the contents of `as` if non-empty, otherwise, the zero element of the `Monoid` for the type `B`.
   */
  final def <^>[A, B: Monoid](as: List[A])(f: NonEmptyList[A] => B): B = as match {
    case Nil    => Monoid[B].zero
    case h :: t => f(NonEmptyList.nel(h, t))
  }

  /** Run `p(a)`s and collect `as` while `p` yields true.  Don't run
    * any `p`s after the first false.
    */
  final def takeWhileM[A, M[_] : Monad](as: List[A])(p: A => M[Boolean]): M[List[A]] = as match {
    case Nil    => Monad[M].point(Nil)
    case h :: t => Monad[M].bind(p(h))(b =>
      if (b) Monad[M].map(takeWhileM(t)(p))((tt: List[A]) => h :: tt) else Monad[M].point(Nil))
  }

  /** Run `p(a)`s and collect `as` while `p` yields false.  Don't run
    * any `p`s after the first true.
    */
  final def takeUntilM[A, M[_] : Monad](as: List[A])(p: A => M[Boolean]): M[List[A]] =
    takeWhileM(as)((a: A) => Monad[M].map(p(a))((b) => !b))

  final def filterM[A, M[_] : Applicative](as: List[A])(p: A => M[Boolean]): M[List[A]] =
    Applicative[M].filterM(as)(p)

  /** Run `p(a)`s left-to-right until it yields a true value,
    * answering `Some(that)`, or `None` if nothing matched `p`.
    */
  final def findM[A, M[_] : Monad](as: List[A])(p: A => M[Boolean]): M[Option[A]] = as match {
    case Nil    => Monad[M].point(None: Option[A])
    case h :: t => Monad[M].bind(p(h))(b =>
      if (b) Monad[M].point(Some(h): Option[A]) else findM(t)(p))
  }

  final def powerset[A](as: List[A]): List[List[A]] = {
    import list.listInstance

    filterM(as)(_ => scala.List(true, false))
  }

  /** A pair of passing and failing values of `as` against `p`. */
  final def partitionM[A, M[_]](as: List[A])(p: A => M[Boolean])(implicit F: Applicative[M]): M[(List[A], List[A])] = as match {
    case Nil    => F.point(Nil: List[A], Nil: List[A])
    case h :: t =>
      F.ap(partitionM(t)(p))(F.map(p(h))(b => {
          case (x, y) => if (b) (h :: x, y) else (x, h :: y)
      }))
  }

  /** A pair of the longest prefix of passing `as` against `p`, and
    * the remainder. */
  final def spanM[A, M[_] : Monad](as: List[A])(p: A => M[Boolean]): M[(List[A], List[A])] = as match {
    case Nil    => Monad[M].point(Nil, Nil)
    case h :: t =>
      Monad[M].bind(p(h))(b =>
        if (b) Monad[M].map(spanM(t)(p))((k: (List[A], List[A])) => (h :: k._1, k._2))
        else Monad[M].point(Nil, as))

  }

  /** `spanM` with `p`'s complement. */
  final def breakM[A, M[_] : Monad](as: List[A])(p: A => M[Boolean]): M[(List[A], List[A])] =
    spanM(as)(a => Monad[M].map(p(a))((b: Boolean) => !b))

  @deprecated("use groupWhenM", "7.1")
  final def groupByM[A, M[_] : Monad](as: List[A])(p: (A, A) => M[Boolean]): M[List[List[A]]] = groupWhenM(as)(p)
  /** Split at each point where `p(as(n), as(n+1))` yields false. */
  final def groupWhenM[A, M[_] : Monad](as: List[A])(p: (A, A) => M[Boolean]): M[List[List[A]]] = as match {
    case Nil    => Monad[M].point(Nil)
    case h :: t => {
      Monad[M].bind(spanM(t)(p(h, _))) {
        case (x, y) =>
          Monad[M].map(groupWhenM(y)(p))((g: List[List[A]]) => (h :: x) :: g)
      }
    }
  }
  
  /** As with the standard library `groupBy` but preserving the fact that the values in the Map must be non-empty  */   
  final def groupBy1[A, B](as: List[A])(f: A => B): Map[B, NonEmptyList[A]] = (Map.empty[B, NonEmptyList[A]] /: as) { (nels, a) =>
    val b = f(a)
    nels + (b -> (nels get b map (a <:: _) getOrElse NonEmptyList(a)))
  } mapValues (_.reverse) 

  /** `groupWhenM` specialized to [[scalaz.Id.Id]]. */
  final def groupWhen[A](as: List[A])(p: (A, A) => Boolean): List[List[A]] =
    groupWhenM(as)((a1: A, a2: A) => p(a1, a2): Id[Boolean])

  /** All of the `B`s, in order, and the final `C` acquired by a
    * stateful left fold over `as`. */
  final def mapAccumLeft[A, B, C](as: List[A])(c: C, f: (C, A) => (C, B)): (C, List[B]) = as match {
    case Nil    => (c, Nil)
    case h :: t => {
      val (i, j) = f(c, h)
      val (k, l) = mapAccumLeft(t)(i, f)
      (k, j :: l)
    }
  }

  /** All of the `B`s, in order `as`-wise, and the final `C` acquired
    * by a stateful right fold over `as`. */
  final def mapAccumRight[A, B, C](as: List[A])(c: C, f: (C, A) => (C, B)): (C, List[B]) = as match {
    case Nil    => (c, Nil)
    case h :: t => {
      val (i, j) = mapAccumRight(t)(c, f)
      val (k, l) = f(i, h)
      (k, l :: j)
    }
  }

  /** `[as, as.tail, as.tail.tail, ..., Nil]` */
  final def tailz[A](as: List[A]): List[List[A]] = as match {
    case Nil           => scala.List(Nil)
    case xxs@(_ :: xs) => xxs :: tailz(xs)
  }

  /** `[Nil, as take 1, as take 2, ..., as]` */
  final def initz[A](as: List[A]): List[List[A]] = as match {
    case Nil           => scala.List(Nil)
    case xxs@(x :: xs) => Nil :: (initz(xs) map (x :: _))
  }

  /** Combinations of `as` and `as`, excluding same-element pairs. */
  final def allPairs[A](as: List[A]): List[(A, A)] =
    tailz(as).tail flatMap (as zip _)

  /** `[(as(0), as(1)), (as(1), as(2)), ... (as(size-2), as(size-1))]` */
  final def adjacentPairs[A](as: List[A]): List[(A, A)] = as match {
    case Nil      => Nil
    case (_ :: t) => as zip t
  }
}

object list extends ListInstances with ListFunctions {
  object listSyntax extends scalaz.syntax.std.ToListOps
}


trait ListEqual[A] extends Equal[List[A]] {
  implicit def A: Equal[A]

  override def equalIsNatural: Boolean = A.equalIsNatural

  override def equal(a1: List[A], a2: List[A]) = (a1 corresponds a2)(Equal[A].equal)
}

trait ListOrder[A] extends Order[List[A]] with ListEqual[A] {
  implicit def A: Order[A]

  import Ordering._

  def order(a1: List[A], a2: List[A]) =
    (a1, a2) match {
      case (Nil, Nil)     => EQ
      case (Nil, _::_)    => LT
      case (_::_, Nil)    => GT
      case (a::as, b::bs) => Order[A].order(a, b) match {
        case EQ => order(as, bs)
        case x  => x
      }
    }

}
