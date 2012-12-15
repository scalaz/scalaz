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

trait IndexedSeqInstances0 {
  implicit def indexedSeqEqual[A](implicit A0: Equal[A]) = new IndexedSeqEqual[A, IndexedSeq[A]] {
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
    def each[A](fa: IxSq[A])(f: (A) => Unit) = fa foreach f
    def index[A](fa: IxSq[A], i: Int) = if (fa.size > i) Some(fa(i)) else None
    def length[A](fa: IxSq[A]) = fa.length
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

  final def intercalate[A](as1: IxSq[IxSq[A]], as2: IxSq[A]): IxSq[A] = intersperse(as1, as2).flatten

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

  final def takeWhileM[A, M[_] : Monad](as: IxSq[A])(p: A => M[Boolean]): M[IxSq[A]] =
    lazyFoldRight(as, Monad[M].point(empty[A]))((a, as) =>
      Monad[M].bind(p(a))(b =>
        if (b) Monad[M].map(as)((tt: IxSq[A]) => a +: tt)
        else Monad[M].point(empty)))

  final def takeUntilM[A, M[_] : Monad](as: IxSq[A])(p: A => M[Boolean]): M[IxSq[A]] =
    takeWhileM(as)((a: A) => Monad[M].map(p(a))((b) => !b))

  final def filterM[A, M[_] : Monad](as: IxSq[A])(p: A => M[Boolean]): M[IxSq[A]] =
    lazyFoldRight(as, Monad[M].point(empty[A]))((a, g) =>
      Monad[M].bind(p(a))(b => if (b) Monad[M].map(g)(tt => a +: tt) else g))

  final def findM[A, M[_] : Monad](as: IxSq[A])(p: A => M[Boolean]): M[Option[A]] =
    lazyFoldRight(as, Monad[M].point(None: Option[A]))((a, g) =>
      Monad[M].bind(p(a))(b =>
        if (b) Monad[M].point(Some(a): Option[A]) else g))

  final def powerset[A](as: IxSq[A]): IxSq[IxSq[A]] = {
    implicit val indexedSeqInstance = covariant
    val tf = empty[Boolean] :+ true :+ false
    filterM(as)(_ => tf)
  }

  final def partitionM[A, M[_] : Monad](as: IxSq[A])(p: A => M[Boolean]): M[(IxSq[A], IxSq[A])] =
    lazyFoldRight(as, Monad[M].point(empty[A], empty[A]))((a, g) =>
      Monad[M].bind(p(as.head))(b =>
        Monad[M].map(g) {
          case (x, y) => if (b) (a +: x, y) else (x, a +: y)
        }
      ))

  final def spanM[A, M[_] : Monad](as: IxSq[A])(p: A => M[Boolean]): M[(IxSq[A], IxSq[A])] =
    lazyFoldRight(as, Monad[M].point(empty[A], empty[A]))((a, g) =>
      Monad[M].bind(p(a))(b =>
        if (b) Monad[M].map(g)((k: (IxSq[A], IxSq[A])) => (as.head +: k._1, k._2))
        else Monad[M].point(empty, as)))

  final def breakM[A, M[_] : Monad](as: IxSq[A])(p: A => M[Boolean]): M[(IxSq[A], IxSq[A])] =
    spanM(as)(a => Monad[M].map(p(a))((b: Boolean) => !b))

  final def groupByM[A, M[_] : Monad](as: IxSq[A])(p: (A, A) => M[Boolean]): M[IxSq[IxSq[A]]] =
    if (as.isEmpty) Monad[M].point(empty) else
      Monad[M].bind(spanM(as.tail)(p(as.head, _))) {
        case (x, y) =>
          Monad[M].map(groupByM(y)(p))((g: IxSq[IxSq[A]]) => (as.head +: x) +: g)
      }

  final def groupWhen[A](as: IxSq[A])(p: (A, A) => Boolean): IxSq[IxSq[A]] =
    groupByM(as)((a1: A, a2: A) => p(a1, a2): Id[Boolean])

  final def mapAccumLeft[A, B, C](as: IxSq[A])(c: C, f: (C, A) => (C, B)): (C, IxSq[B]) =
    as.foldLeft((c, empty[B])){(acc, a) => acc match {
      case (c, v) => f(c, a) match {
        case (c, b) => (c, v :+ b)
      }}
    }

  final def mapAccumRight[A, B, C](as: IxSq[A])(c: C, f: (C, A) => (C, B)): (C, IxSq[B]) =
    if (as.isEmpty) (c, empty) else {
      val (i, j) = mapAccumRight(as.tail)(c, f)
      val (k, v) = f(i, as.head)
      (k, v +: j)
    }

  final def tailz[A](as: IxSq[A]): IxSq[IxSq[A]] =
    if (as.isEmpty) empty[A] +: empty else as +: tailz(as.tail)

  final def initz[A](as: IxSq[A]): IxSq[IxSq[A]] =
    empty +: (if (as.isEmpty) empty else (initz(as.tail) map (as.head +: _)))

  final def allPairs[A](as: IxSq[A]): IxSq[(A, A)] =
    tailz(as).tail flatMap (as zip _)

  final def adjacentPairs[A](as: IxSq[A]): IxSq[(A, A)] =
    if (as.isEmpty) empty else as zip as.tail
}

object indexedSeq extends IndexedSeqInstances with IndexedSeqSubFunctions with IndexedSeqSubIndexedSeq {
  object indexedSeqSyntax extends scalaz.syntax.std.ToIndexedSeqOps
}

trait IndexedSeqEqual[A, Coll <: IndexedSeq[A]] extends Equal[Coll] {
  implicit def A: Equal[A]

  override def equalIsNatural: Boolean = A.equalIsNatural

  override def equal(a1: Coll, a2: Coll) = (a1 corresponds a2)(Equal[A].equal)
}

trait IndexedSeqSubOrder[A, Coll <: IndexedSeq[A] with IndexedSeqLike[A, Coll]] extends Order[Coll] with IndexedSeqEqual[A, Coll] {
  implicit def A: Order[A]

  import Ordering._

  def order(a1: Coll, a2: Coll): Ordering =
    (a1, a2) match {
      case (IndexedSeq(), IndexedSeq()) => EQ
      case (IndexedSeq(), y)        => LT
      case (x, IndexedSeq())        => GT
      case (as, bs) => Order[A].order(as.head, bs.head) match {
        case EQ => order(as.tail, bs.tail)
        case x  => x
      }
    }

}
