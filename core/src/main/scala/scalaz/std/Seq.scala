package scalaz
package std

import scalaz.Id._
import annotation.tailrec

trait SeqInstances0 {
  implicit def seqEqual[A](implicit A0: Equal[A]) = new SeqEqual[A] {
    implicit def A = A0
  }
}

trait SeqInstances extends SeqInstances0 {
  implicit val seqInstances = new Traverse[Seq] with MonadPlus[Seq] with Each[Seq] with Index[Seq] with Length[Seq] with Zip[Seq] with Unzip[Seq] with IsEmpty[Seq] {
    def each[A](fa: Seq[A])(f: A ⇒ Unit) = fa foreach f
    def index[A](fa: Seq[A], i: Int) = if (fa.size > i) Some(fa(i)) else None
    def length[A](fa: Seq[A]) = fa.length
    def point[A](a: ⇒ A) = Seq(a)
    def bind[A, B](fa: Seq[A])(f: A ⇒ Seq[B]) = fa flatMap f
    def empty[A] = Seq()
    def plus[A](a: Seq[A], b: ⇒ Seq[A]) = a ++ b
    def isEmpty[A](a: Seq[A]) = a.isEmpty
    override def map[A, B](fa: Seq[A])(f: A ⇒ B) = fa map f

    def zip[A, B](a: ⇒ Seq[A], b: ⇒ Seq[B]) = a zip b
    def unzip[A, B](a: Seq[(A, B)]) = a.unzip

    def traverseImpl[F[_], A, B](fa: Seq[A])(f: A ⇒ F[B])(implicit F: Applicative[F]) =
      DList.fromList(fa.toList).foldr(F.point(Seq[B]())) { (a, fbs) ⇒
        F.apply2(f(a), fbs)(_ +: _)
      }

    override def traverseS[S, A, B](v: Seq[A])(f: A ⇒ State[S,B]): State[S, Seq[B]] =
      State((s: S) ⇒
        v.foldLeft((s, Seq[B]()))((acc, a) ⇒ {
          val bs = f(a)(acc._1)
          (bs._1, acc._2 :+ bs._2)
        }))

    override def foldRight[A, B](fa: Seq[A], z: ⇒ B)(f: (A, ⇒ B) ⇒ B) = {
      var i = fa.length
      var r = z
      while (i > 0) {
        i -= 1
        // force and copy the value of r to ensure correctness
        val w = r
        r = f(fa(i), w)
      }
      r
    }
  }

  implicit def seqMonoid[A]: Monoid[Seq[A]] = new Monoid[Seq[A]] {
    def append(f1: Seq[A], f2: ⇒ Seq[A]) = f1 ++ f2
    def zero: Seq[A] = Seq()
  }

  implicit def seqShow[A: Show]: Show[Seq[A]] = new Show[Seq[A]] {
    override def show(as: Seq[A]) = Cord("[", Cord.mkCord(",", as.map(Show[A].show(_)):_*), "]")
  }

  implicit def seqOrder[A](implicit A0: Order[A]): Order[Seq[A]] = new SeqOrder[A] {
    implicit def A = A0
  }
}

trait SeqFunctions {
  @inline private[this] final
  def lazyFoldRight[A, B](as: Seq[A], b: ⇒ B)(f: (A, ⇒ B) ⇒ B) = {
    def rec(ix: Int): B =
      if (ix >= as.length - 1) b else f(as(ix+1), rec(ix+1))
    rec(-1)
  }

  /** Intersperse the element `a` between each adjacent pair of elements in `as` */
  final def intersperse[A](as: Seq[A], a: A): Seq[A] =
    if (as.isEmpty) Seq() else as.init.foldRight(as.last +: Seq())(_ +: a +: _)

  final def toNel[A](as: Seq[A]): Option[NonEmptyList[A]] =
    if (as.isEmpty) None else Some(NonEmptyList.nel(as.head, as.tail.toList))

  final def toZipper[A](as: Seq[A]): Option[Zipper[A]] =
    stream.toZipper(as.toStream)

  final def zipperEnd[A](as: Seq[A]): Option[Zipper[A]] =
    stream.zipperEnd(as.toStream)

  /**
   * Returns `f` applied to the contents of `as` if non-empty, otherwise, the zero element of the `Monoid` for the type `B`.
   */
  final def <^>[A, B: Monoid](as: Seq[A])(f: NonEmptyList[A] ⇒ B): B =
    if (as.isEmpty) Monoid[B].zero else f(NonEmptyList.nel(as.head, as.tail.toList))

  /** Run `p(a)`s and collect `as` while `p` yields true.  Don't run
    * any `p`s after the first false.
    */
  final def takeWhileM[A, M[_] : Monad](as: Seq[A])(p: A ⇒ M[Boolean]): M[Seq[A]] =
    lazyFoldRight(as, Monad[M].point(Seq[A]()))((a, as) ⇒
      Monad[M].bind(p(a))(b ⇒
        if (b) Monad[M].map(as)((tt: Seq[A]) ⇒ a +: tt)
        else Monad[M].point(Seq())))

  /** Run `p(a)`s and collect `as` while `p` yields false.  Don't run
    * any `p`s after the first true.
    */
  final def takeUntilM[A, M[_] : Monad](as: Seq[A])(p: A ⇒ M[Boolean]): M[Seq[A]] =
    takeWhileM(as)((a: A) ⇒ Monad[M].map(p(a))((b) ⇒ !b))

  final def filterM[A, M[_]](as: Seq[A])(p: A ⇒ M[Boolean])(implicit F: Applicative[M]): M[Seq[A]] =
    lazyFoldRight(as, F.point(Seq[A]()))((a, g) ⇒
      F.ap(g)(F.map(p(a))(b ⇒ t ⇒ if (b) a +: t else t)))

  /** Run `p(a)`s left-to-right until it yields a true value,
    * answering `Some(that)`, or `None` if nothing matched `p`.
    */
  final def findM[A, M[_] : Monad](as: Seq[A])(p: A ⇒ M[Boolean]): M[Option[A]] =
    lazyFoldRight(as, Monad[M].point(None: Option[A]))((a, g) ⇒
      Monad[M].bind(p(a))(b ⇒
        if (b) Monad[M].point(Some(a): Option[A]) else g))

  final def powerset[A](as: Seq[A]): Seq[Seq[A]] = {
    import seq.seqInstances
    val tf = Seq[Boolean]() :+ true :+ false
    filterM(as)(_ ⇒ tf)
  }

  /** A pair of passing and failing values of `as` against `p`. */
  final def partitionM[A, M[_]](as: Seq[A])(p: A ⇒ M[Boolean])(implicit F: Applicative[M]): M[(Seq[A], Seq[A])] =
    lazyFoldRight(as, F.point(Seq[A](), Seq[A]()))((a, g) ⇒
      F.ap(g)(F.map(p(a))(b ⇒ {
        case (x, y) ⇒ if (b) (a +: x, y) else (x, a +: y)
      })))

  /** A pair of the longest prefix of passing `as` against `p`, and
    * the remainder. */
  final def spanM[A, M[_] : Monad](as: Seq[A])(p: A ⇒ M[Boolean]): M[(Seq[A], Seq[A])] =
    Monad[M].map(takeWhileM(as)(p))(ys ⇒ (ys, as drop (ys.length)))

  /** `spanM` with `p`'s complement. */
  final def breakM[A, M[_] : Monad](as: Seq[A])(p: A ⇒ M[Boolean]): M[(Seq[A], Seq[A])] =
    spanM(as)(a ⇒ Monad[M].map(p(a))((b: Boolean) ⇒ !b))

  /** Split at each point where `p(as(n), as(n+1))` yields false. */
  final def groupByM[A, M[_] : Monad](as: Seq[A])(p: (A, A) ⇒ M[Boolean]): M[Seq[Seq[A]]] =
    if (as.isEmpty) Monad[M].point(Seq()) else
      Monad[M].bind(spanM(as.tail)(p(as.head, _))) {
        case (x, y) ⇒
          Monad[M].map(groupByM(y)(p))((g: Seq[Seq[A]]) ⇒ (as.head +: x) +: g)
      }

  /** `groupByM` specialized to [[scalaz.Id.Id]]. */
  final def groupWhen[A](as: Seq[A])(p: (A, A) ⇒ Boolean): Seq[Seq[A]] =
    groupByM(as)((a1: A, a2: A) ⇒ p(a1, a2): Id[Boolean])

  /** All of the `B`s, in order, and the final `C` acquired by a
    * stateful left fold over `as`. */
  final def mapAccumLeft[A, B, C](as: Seq[A])(c: C, f: (C, A) ⇒ (C, B)): (C, Seq[B]) =
    as.foldLeft((c, Seq[B]())){(acc, a) ⇒ acc match {
      case (c, v) ⇒ f(c, a) match {
        case (c, b) ⇒ (c, v :+ b)
      }}
    }

  /** All of the `B`s, in order `as`-wise, and the final `C` acquired
    * by a stateful right fold over `as`. */
  final def mapAccumRight[A, B, C](as: Seq[A])(c: C, f: (C, A) ⇒ (C, B)): (C, Seq[B]) =
    as.foldRight((c, Seq[B]())){(a, acc) ⇒ acc match {
      case (c, v) ⇒ f(c, a) match {
        case (c, b) ⇒ (c, b +: v)
      }}
    }

  /** `[as, as.tail, as.tail.tail, ..., `empty Seq`]` */
  final def tailz[A](as: Seq[A]): Seq[Seq[A]] =
    if (as.isEmpty) Seq[A]() +: Seq() else as +: tailz(as.tail)

  /** `[`empty Seq`, as take 1, as take 2, ..., as]` */
  final def initz[A](as: Seq[A]): Seq[Seq[A]] = {
    @tailrec def rec(acc: Seq[Seq[A]], as: Seq[A]): Seq[Seq[A]] =
      if (as.isEmpty) as +: acc else rec(as +: acc, as.init)
    rec(Seq(), as)
  }

  /** Combinations of `as` and `as`, excluding same-element pairs. */
  final def allPairs[A](as: Seq[A]): Seq[(A, A)] =
    tailz(as).tail flatMap (as zip _)

  /** `[(as(0), as(1)), (as(1), as(2)), ... (as(size-2), as(size-1))]` */
  final def adjacentPairs[A](as: Seq[A]): Seq[(A, A)] =
    if (as.isEmpty) Seq() else as zip as.tail
}

object seq extends SeqInstances with SeqFunctions {
  object seqSyntax extends scalaz.syntax.std.ToSeqOps
}

trait SeqEqual[A] extends Equal[Seq[A]] {
  implicit def A: Equal[A]

  override def equalIsNatural: Boolean = A.equalIsNatural

  override def equal(a1: Seq[A], a2: Seq[A]) = (a1 corresponds a2)(Equal[A].equal)
}

trait SeqOrder[A] extends Order[Seq[A]] with SeqEqual[A] {
  implicit def A: Order[A]

  import Ordering._

  def order(a1: Seq[A], a2: Seq[A]) = {
    val a1s = a1.length
    @tailrec def receqs(ix: Int): Ordering =
      if (ix >= a1s) EQ else A.order(a1(ix), a2(ix)) match {
        case EQ ⇒ receqs(ix + 1)
        case other ⇒ other
      }
    Semigroup[Ordering].append(Order[Int].order(a1s, a2.length), receqs(0))
  }
}
