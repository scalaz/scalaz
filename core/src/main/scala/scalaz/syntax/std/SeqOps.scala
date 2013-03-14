package scalaz
package syntax
package std

import scalaz.std.{seq ⇒ s}

trait SeqOps[A] extends Ops[Seq[A]] {
  final def intersperse(a: A): Seq[A] = s.intersperse(self, a)
  final def toNel: Option[NonEmptyList[A]] = s.toNel(self)

  final def toZipper: Option[Zipper[A]] = s.toZipper(self)

  final def zipperEnd: Option[Zipper[A]] = s.zipperEnd(self)

  final def <^>[B: Monoid](f: NonEmptyList[A] ⇒ B): B = s.<^>(self)(f)

  final def takeWhileM[M[_] : Monad](p: A ⇒ M[Boolean]): M[Seq[A]] = s.takeWhileM(self)(p)

  final def takeUntilM[M[_] : Monad](p: A ⇒ M[Boolean]): M[Seq[A]] = s.takeUntilM(self)(p)

  final def filterM[M[_] : Applicative](p: A ⇒ M[Boolean]): M[Seq[A]] = s.filterM(self)(p)

  final def findM[M[_] : Monad](p: A ⇒ M[Boolean]): M[Option[A]] = s.findM(self)(p)

  final def powerset: Seq[Seq[A]] = s.powerset(self)

  final def partitionM[M[_] : Applicative](p: A ⇒ M[Boolean]): M[(Seq[A], Seq[A])] = s.partitionM(self)(p)

  final def spanM[M[_] : Monad](p: A ⇒ M[Boolean]): M[(Seq[A], Seq[A])] = s.spanM(self)(p)

  final def breakM[M[_] : Monad](p: A ⇒ M[Boolean]): M[(Seq[A], Seq[A])] = s.breakM(self)(p)

  final def groupByM[M[_] : Monad](p: (A, A) ⇒ M[Boolean]): M[Seq[Seq[A]]] = s.groupByM(self)(p)

  final def groupWhen(p: (A, A) ⇒ Boolean): Seq[Seq[A]] = s.groupWhen(self)(p)

  final def mapAccumLeft[B, C](c: C, f: (C, A) ⇒ (C, B)): (C, Seq[B]) = s.mapAccumLeft(self)(c, f)

  final def mapAccumRight[B, C](c: C, f: (C, A) ⇒ (C, B)): (C, Seq[B]) = s.mapAccumRight(self)(c, f)

  final def tailz: Seq[Seq[A]] = s.tailz(self)

  final def initz: Seq[Seq[A]] = s.initz(self)

  final def allPairs: Seq[(A, A)] = s.allPairs(self)

  final def adjacentPairs: Seq[(A, A)] = s.adjacentPairs(self)
}

trait ToSeqOps {
  implicit def ToSeqOpsFromSeq[A](a: Seq[A]): SeqOps[A] = new SeqOps[A] {
    protected def v = scalaz.std.indexedSeq
    val self = a
  }
}
