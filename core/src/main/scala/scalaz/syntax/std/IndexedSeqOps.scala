package scalaz
package syntax
package std

import collection.immutable.IndexedSeq

trait IndexedSeqOps[IS[+_], A] extends Ops[IS[A]] {

  protected def v: scalaz.std.IndexedSeqSubFunctions {
    type IxSq[+X] = IS[X]
  }

  final def intersperse(a: A): IS[A] = v.intersperse(self, a)
  final def toNel: Option[NonEmptyList[A]] = v.toNel(self)

  final def toZipper: Option[Zipper[A]] = v.toZipper(self)

  final def zipperEnd: Option[Zipper[A]] = v.zipperEnd(self)

  final def <^>[B: Monoid](f: NonEmptyList[A] => B): B = v.<^>(self)(f)

  final def takeWhileM[M[_] : Monad](p: A => M[Boolean]): M[IS[A]] = v.takeWhileM(self)(p)

  final def takeUntilM[M[_] : Monad](p: A => M[Boolean]): M[IS[A]] = v.takeUntilM(self)(p)

  final def filterM[M[_] : Applicative](p: A => M[Boolean]): M[IS[A]] = v.filterM(self)(p)

  final def findM[M[_] : Monad](p: A => M[Boolean]): M[Option[A]] = v.findM(self)(p)

  final def powerset: IS[IS[A]] = v.powerset(self)

  final def partitionM[M[_] : Applicative](p: A => M[Boolean]): M[(IS[A], IS[A])] = v.partitionM(self)(p)

  final def spanM[M[_] : Monad](p: A => M[Boolean]): M[(IS[A], IS[A])] = v.spanM(self)(p)

  final def breakM[M[_] : Monad](p: A => M[Boolean]): M[(IS[A], IS[A])] = v.breakM(self)(p)

  @deprecated("use groupWhenM", "7.1")
  final def groupByM[M[_] : Monad](p: (A, A) => M[Boolean]): M[IS[IS[A]]] = v.groupWhenM(self)(p)
  final def groupWhenM[M[_] : Monad](p: (A, A) => M[Boolean]): M[IS[IS[A]]] = v.groupWhenM(self)(p)

  final def groupWhen(p: (A, A) => Boolean): IS[IS[A]] = v.groupWhen(self)(p)

  final def mapAccumLeft[B, C](c: C, f: (C, A) => (C, B)): (C, IS[B]) = v.mapAccumLeft(self)(c, f)

  final def mapAccumRight[B, C](c: C, f: (C, A) => (C, B)): (C, IS[B]) = v.mapAccumRight(self)(c, f)

  final def tailz: IS[IS[A]] = v.tailz(self)

  final def initz: IS[IS[A]] = v.initz(self)

  final def allPairs: IS[(A, A)] = v.allPairs(self)

  final def adjacentPairs: IS[(A, A)] = v.adjacentPairs(self)
}

trait ToIndexedSeqOps {
  implicit def ToIndexedSeqOpsFromIndexedSeq[A](a: IndexedSeq[A]): IndexedSeqOps[IndexedSeq, A] = new IndexedSeqOps[IndexedSeq, A] {
    protected def v = scalaz.std.indexedSeq
    val self = a
  }
}
