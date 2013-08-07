package scalaz
package std

import scalaz.Id._
import annotation.tailrec
import collection.immutable.IndexedSeq
import collection.IndexedSeqLike
import collection.generic.{CanBuildFrom, GenericTraversableTemplate}

trait IndexedSeqSub {
  type IxSq[+A] <: IndexedSeq[A] with GenericTraversableTemplate[A, IxSq] with IndexedSeqLike[A, IxSq[A]]
  protected implicit def buildIxSq[A, B]: CanBuildFrom[IxSq[A], B, IxSq[B]]
  protected def covariant: Traverse[IxSq] with Monad[IxSq]
  protected def empty[A]: IxSq[A]
}

trait IndexedSeqSubIndexedSeq extends IndexedSeqSub {
  type IxSq[+A] = IndexedSeq[A]
  protected final def buildIxSq[A, B] = implicitly
  protected final def covariant = indexedSeq.indexedSeqInstance
  protected final def empty[A] = IndexedSeq()
}

sealed trait IndexedSeqInstances0 {
  implicit def indexedSeqEqual[A](implicit A0: Equal[A]): Equal[IndexedSeq[A]] = new IndexedSeqEqual[A, IndexedSeq[A]] {
    implicit def A = A0
  }
}

trait IndexedSeqInstances extends IndexedSeqInstances0 {
  object generic extends IndexedSeqSubIndexedSeq with IndexedSeqSubInstances

  implicit val indexedSeqInstance = generic.ixSqInstance

  implicit def indexedSeqMonoid[A]: Monoid[IndexedSeq[A]] = generic.ixSqMonoid

  implicit def indexedSeqShow[A: Show]: Show[IndexedSeq[A]] = generic.ixSqShow

  implicit def indexedSeqOrder[A](implicit A0: Order[A]): Order[IndexedSeq[A]] = generic.ixSqOrder
}

trait IndexedSeqSubInstances extends IndexedSeqInstances0 with IndexedSeqSub {self =>
  val ixSqInstance = new Traverse[IxSq] with MonadPlus[IxSq] with Each[IxSq] with Index[IxSq] with Length[IxSq] with Zip[IxSq] with Unzip[IxSq] with IsEmpty[IxSq] {
    def each[A](fa: IxSq[A])(f: A => Unit) = fa foreach f
    override def index[A](fa: IxSq[A], i: Int) = fa.lift.apply(i)
    // TODO remove after removal of Index
    override def indexOr[A](fa: IxSq[A], default: => A, i: Int) = super[Traverse].indexOr(fa, default, i)
    override def length[A](fa: IxSq[A]) = fa.length
    def point[A](a: => A) = empty :+ a
    def bind[A, B](fa: IxSq[A])(f: A => IxSq[B]) = fa flatMap f
    def empty[A] = self.empty[A]
    def plus[A](a: IxSq[A], b: => IxSq[A]) = a ++ b
    def isEmpty[A](a: IxSq[A]) = a.isEmpty
    override def map[A, B](v: IxSq[A])(f: A => B) = v map f

    def zip[A, B](a: => IxSq[A], b: => IxSq[B]) = a zip b
    def unzip[A, B](a: IxSq[(A, B)]) = a.unzip

    def traverseImpl[F[_], A, B](v: IxSq[A])(f: A => F[B])(implicit F: Applicative[F]) = {
      DList.fromList(v.toList).foldr(F.point(empty[B])) {
         (a, fbs) => F.apply2(f(a), fbs)(_ +: _)
      }
    }

    override def traverseS[S,A,B](v: IxSq[A])(f: A => State[S,B]): State[S,IxSq[B]] =
      State((s: S) =>
        v.foldLeft((s, empty[B]))((acc, a) => {
          val bs = f(a)(acc._1)
          (bs._1, acc._2 :+ bs._2)
        }))

    override def toIndexedSeq[A](fa: IxSq[A]) = fa

    override def foldRight[A, B](fa: IxSq[A], z: => B)(f: (A, => B) => B) = {
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

  implicit def ixSqMonoid[A]: Monoid[IxSq[A]] = new Monoid[IxSq[A]] {
    def append(f1: IxSq[A], f2: => IxSq[A]) = f1 ++ f2
    def zero: IxSq[A] = empty
  }

  implicit def ixSqShow[A: Show]: Show[IxSq[A]] = new Show[IxSq[A]] {
    import Cord._
    override def show(as: IxSq[A]) =
      Cord("[", mkCord(",", as.map(Show[A].show(_)):_*), "]")
  }

  implicit def ixSqOrder[A](implicit A0: Order[A]): Order[IxSq[A]] = new IndexedSeqSubOrder[A, IxSq[A]] {
    implicit def A = A0
  }

}

trait IndexedSeqSubFunctions extends IndexedSeqSub {
  @inline private[this] final
  def lazyFoldRight[A, B](as: IxSq[A], b: => B)(f: (A, => B) => B) = {
    def rec(ix: Int): B =
      if (ix >= as.length - 1) b else f(as(ix+1), rec(ix+1))
    rec(-1)
  }

  /** Intersperse the element `a` between each adjacent pair of elements in `as` */
  final def intersperse[A](as: IxSq[A], a: A): IxSq[A] =
    if (as.isEmpty) empty else as.init.foldRight(as.last +: empty)(_ +: a +: _)

  final def toNel[A](as: IxSq[A]): Option[NonEmptyList[A]] =
    if (as.isEmpty) None else Some(NonEmptyList.nel(as.head, as.tail.toList))

  final def toZipper[A](as: IxSq[A]): Option[Zipper[A]] =
    stream.toZipper(as.toStream)

  final def zipperEnd[A](as: IxSq[A]): Option[Zipper[A]] =
    stream.zipperEnd(as.toStream)

  /**
   * Returns `f` applied to the contents of `as` if non-empty, otherwise, the zero element of the `Monoid` for the type `B`.
   */
  final def <^>[A, B: Monoid](as: IxSq[A])(f: NonEmptyList[A] => B): B =
    if (as.isEmpty) Monoid[B].zero else f(NonEmptyList.nel(as.head, as.tail.toList))

  /** Run `p(a)`s and collect `as` while `p` yields true.  Don't run
    * any `p`s after the first false.
    */
  final def takeWhileM[A, M[_] : Monad](as: IxSq[A])(p: A => M[Boolean]): M[IxSq[A]] =
    lazyFoldRight(as, Monad[M].point(empty[A]))((a, as) =>
      Monad[M].bind(p(a))(b =>
        if (b) Monad[M].map(as)((tt: IxSq[A]) => a +: tt)
        else Monad[M].point(empty)))

  /** Run `p(a)`s and collect `as` while `p` yields false.  Don't run
    * any `p`s after the first true.
    */
  final def takeUntilM[A, M[_] : Monad](as: IxSq[A])(p: A => M[Boolean]): M[IxSq[A]] =
    takeWhileM(as)((a: A) => Monad[M].map(p(a))((b) => !b))

  final def filterM[A, M[_]](as: IxSq[A])(p: A => M[Boolean])(implicit F: Applicative[M]): M[IxSq[A]] =
    lazyFoldRight(as, F.point(empty[A]))((a, g) =>
      F.ap(g)(F.map(p(a))(b => t => if (b) a +: t else t)))

  /** Run `p(a)`s left-to-right until it yields a true value,
    * answering `Some(that)`, or `None` if nothing matched `p`.
    */
  final def findM[A, M[_] : Monad](as: IxSq[A])(p: A => M[Boolean]): M[Option[A]] =
    lazyFoldRight(as, Monad[M].point(None: Option[A]))((a, g) =>
      Monad[M].bind(p(a))(b =>
        if (b) Monad[M].point(Some(a): Option[A]) else g))

  final def powerset[A](as: IxSq[A]): IxSq[IxSq[A]] = {
    implicit val indexedSeqInstance = covariant
    val tf = empty[Boolean] :+ true :+ false
    filterM(as)(_ => tf)
  }

  /** A pair of passing and failing values of `as` against `p`. */
  final def partitionM[A, M[_]](as: IxSq[A])(p: A => M[Boolean])(implicit F: Applicative[M]): M[(IxSq[A], IxSq[A])] =
    lazyFoldRight(as, F.point(empty[A], empty[A]))((a, g) =>
      F.ap(g)(F.map(p(a))(b => {
        case (x, y) => if (b) (a +: x, y) else (x, a +: y)
      })))

  /** A pair of the longest prefix of passing `as` against `p`, and
    * the remainder. */
  final def spanM[A, M[_] : Monad](as: IxSq[A])(p: A => M[Boolean]): M[(IxSq[A], IxSq[A])] =
    Monad[M].map(takeWhileM(as)(p))(ys => (ys, as drop (ys.length)))

  /** `spanM` with `p`'s complement. */
  final def breakM[A, M[_] : Monad](as: IxSq[A])(p: A => M[Boolean]): M[(IxSq[A], IxSq[A])] =
    spanM(as)(a => Monad[M].map(p(a))((b: Boolean) => !b))

  @deprecated("use groupWhenM", "7.1")
  final def groupByM[A, M[_] : Monad](as: IxSq[A])(p: (A, A) => M[Boolean]): M[IxSq[IxSq[A]]] =
    groupWhenM(as)(p)

  /** Split at each point where `p(as(n), as(n+1))` yields false. */
  final def groupWhenM[A, M[_] : Monad](as: IxSq[A])(p: (A, A) => M[Boolean]): M[IxSq[IxSq[A]]] =
    if (as.isEmpty)
      Monad[M].point(empty)
    else
      Monad[M].bind(spanM(as.tail)(p(as.head, _))) {
        case (x, y) =>
          Monad[M].map(groupWhenM(y)(p))((g: IxSq[IxSq[A]]) => (as.head +: x) +: g)
      }

  /** `groupWhenM` specialized to [[scalaz.Id.Id]]. */
  final def groupWhen[A](as: IxSq[A])(p: (A, A) => Boolean): IxSq[IxSq[A]] =
    groupWhenM(as)((a1: A, a2: A) => p(a1, a2): Id[Boolean])

  /** All of the `B`s, in order, and the final `C` acquired by a
    * stateful left fold over `as`. */
  final def mapAccumLeft[A, B, C](as: IxSq[A])(c: C, f: (C, A) => (C, B)): (C, IxSq[B]) =
    as.foldLeft((c, empty[B])){(acc, a) => acc match {
      case (c, v) => f(c, a) match {
        case (c, b) => (c, v :+ b)
      }}
    }

  /** All of the `B`s, in order `as`-wise, and the final `C` acquired
    * by a stateful right fold over `as`. */
  final def mapAccumRight[A, B, C](as: IxSq[A])(c: C, f: (C, A) => (C, B)): (C, IxSq[B]) =
    as.foldRight((c, empty[B])){(a, acc) => acc match {
      case (c, v) => f(c, a) match {
        case (c, b) => (c, b +: v)
      }}
    }

  /** `[as, as.tail, as.tail.tail, ..., `empty IxSq`]` */
  final def tailz[A](as: IxSq[A]): IxSq[IxSq[A]] =
    if (as.isEmpty) empty[A] +: empty else as +: tailz(as.tail)

  /** `[`empty IxSq`, as take 1, as take 2, ..., as]` */
  final def initz[A](as: IxSq[A]): IxSq[IxSq[A]] = {
    @tailrec def rec(acc: IxSq[IxSq[A]], as: IxSq[A]): IxSq[IxSq[A]] =
      if (as.isEmpty) as +: acc else rec(as +: acc, as.init)
    rec(empty, as)
  }

  /** Combinations of `as` and `as`, excluding same-element pairs. */
  final def allPairs[A](as: IxSq[A]): IxSq[(A, A)] =
    tailz(as).tail flatMap (as zip _)

  /** `[(as(0), as(1)), (as(1), as(2)), ... (as(size-2), as(size-1))]` */
  final def adjacentPairs[A](as: IxSq[A]): IxSq[(A, A)] =
    if (as.isEmpty) empty else as zip as.tail
}

object indexedSeq extends IndexedSeqInstances with IndexedSeqSubFunctions with IndexedSeqSubIndexedSeq {
  object indexedSeqSyntax extends scalaz.syntax.std.ToIndexedSeqOps
}

private trait IndexedSeqEqual[A, Coll <: IndexedSeq[A]] extends Equal[Coll] {
  implicit def A: Equal[A]

  override def equalIsNatural: Boolean = A.equalIsNatural

  override def equal(a1: Coll, a2: Coll) = (a1 corresponds a2)(Equal[A].equal)
}

private trait IndexedSeqSubOrder[A, Coll <: IndexedSeq[A] with IndexedSeqLike[A, Coll]] extends Order[Coll] with IndexedSeqEqual[A, Coll] {
  implicit def A: Order[A]

  import Ordering._

  def order(a1: Coll, a2: Coll): Ordering = {
    val a1s = a1.length
    @tailrec def receqs(ix: Int): Ordering =
      if (ix >= a1s) EQ else A.order(a1(ix), a2(ix)) match {
        case EQ => receqs(ix + 1)
        case o => o
      }
    Semigroup[Ordering].append(Order[Int].order(a1s, a2.length),
                               receqs(0))
  }
}
