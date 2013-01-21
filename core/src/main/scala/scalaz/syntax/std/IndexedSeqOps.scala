package scalaz
package syntax
package std

import collection.immutable.IndexedSeq
import scalaz.std.{indexedSeq => v}

trait IndexedSeqOps[A] extends Ops[IndexedSeq[A]] {

  final def intersperse(a: A): IndexedSeq[A] = v.intersperse(self, a)

  final def toNel: Option[NonEmptyList[A]] = v.toNel(self)

  final def toZipper: Option[Zipper[A]] = v.toZipper(self)

  final def zipperEnd: Option[Zipper[A]] = v.zipperEnd(self)

  final def <^>[B: Monoid](f: NonEmptyList[A] => B): B = v.<^>(self)(f)

  final def takeWhileM[M[_] : Monad](p: A => M[Boolean]): M[IndexedSeq[A]] = v.takeWhileM(self)(p)

  final def takeUntilM[M[_] : Monad](p: A => M[Boolean]): M[IndexedSeq[A]] = v.takeUntilM(self)(p)

  final def filterM[M[_] : Applicative](p: A => M[Boolean]): M[IndexedSeq[A]] = v.filterM(self)(p)

  final def findM[M[_] : Monad](p: A => M[Boolean]): M[Option[A]] = v.findM(self)(p)

  final def powerset: IndexedSeq[IndexedSeq[A]] = v.powerset(self)

  final def partitionM[M[_] : Monad](p: A => M[Boolean]): M[(IndexedSeq[A], IndexedSeq[A])] = v.partitionM(self)(p)

  final def spanM[M[_] : Monad](p: A => M[Boolean]): M[(IndexedSeq[A], IndexedSeq[A])] = v.spanM(self)(p)

  final def breakM[M[_] : Monad](p: A => M[Boolean]): M[(IndexedSeq[A], IndexedSeq[A])] = v.breakM(self)(p)

  final def groupByM[M[_] : Monad](p: (A, A) => M[Boolean]): M[IndexedSeq[IndexedSeq[A]]] = v.groupByM(self)(p)

  final def groupWhen(p: (A, A) => Boolean): IndexedSeq[IndexedSeq[A]] = v.groupWhen(self)(p)

  final def mapAccumLeft[B, C](c: C, f: (C, A) => (C, B)): (C, IndexedSeq[B]) = v.mapAccumLeft(self)(c, f)

  final def mapAccumRight[B, C](c: C, f: (C, A) => (C, B)): (C, IndexedSeq[B]) = v.mapAccumRight(self)(c, f)

  final def tailz: IndexedSeq[IndexedSeq[A]] = v.tailz(self)

  final def initz: IndexedSeq[IndexedSeq[A]] = v.initz(self)

  final def allPairs: IndexedSeq[(A, A)] = v.allPairs(self)

  final def adjacentPairs: IndexedSeq[(A, A)] = v.adjacentPairs(self)
}

trait ToIndexedSeqOps {
  implicit def ToIndexedSeqOpsFromIndexedSeq[A](a: IndexedSeq[A]): IndexedSeqOps[A] = new IndexedSeqOps[A] {
    val self = a
  }
}
