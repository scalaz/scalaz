package scalaz
package syntax
package std

import scalaz.std.{list => l}


trait ListOps[A] extends Ops[List[A]] {

  final def intersperse(a: A): List[A] = l.intersperse(self, a)

  final def intercalate[B](other: List[B])(implicit ev: List[A] =:= List[List[B]]): List[B] = l.intercalate(self, other)

  final def toNel: Option[NonEmptyList[A]] = l.toNel(self)

  final def toZipper: Option[Zipper[A]] = l.toZipper(self)

  final def zipperEnd: Option[Zipper[A]] = l.zipperEnd(self)

  final def <^>[B: Monoid](f: NonEmptyList[A] => B): B = l.<^>(self)(f)

  final def takeWhileM[M[_] : Monad](p: A => M[Boolean]): M[List[A]] = l.takeWhileM(self)(p)

  final def takeUntilM[M[_] : Monad](p: A => M[Boolean]): M[List[A]] = l.takeUntilM(self)(p)

  final def filterM[M[_] : Monad](p: A => M[Boolean]): M[List[A]] = l.filterM(self)(p)

  final def findM[M[_] : Monad](p: A => M[Boolean]): M[Option[A]] = l.findM(self)(p)

  final def powerset: List[List[A]] = l.powerset(self)

  final def partitionM[M[_] : Monad](p: A => M[Boolean]): M[(List[A], List[A])] = l.partitionM(self)(p)

  final def spanM[M[_] : Monad](p: A => M[Boolean]): M[(List[A], List[A])] = l.spanM(self)(p)

  final def breakM[M[_] : Monad](p: A => M[Boolean]): M[(List[A], List[A])] = l.breakM(self)(p)

  final def groupByM[M[_] : Monad](p: (A, A) => M[Boolean]): M[List[List[A]]] = l.groupByM(self)(p)

  final def mapAccumLeft[B, C](c: C, f: (C, A) => (C, B)): (C, List[B]) = l.mapAccumLeft(self)(c, f)

  final def mapAccumRight[B, C](c: C, f: (C, A) => (C, B)): (C, List[B]) = l.mapAccumRight(self)(c, f)

  final def tailz: List[List[A]] = l.tailz(self)

  final def initz: List[List[A]] = l.initz(self)

  final def allPairs: List[(A, A)] = l.allPairs(self)

  final def adjacentPairs: List[(A, A)] = l.adjacentPairs(self)
}

trait ToListOps {
  implicit def ToListOpsFromList[A](a: List[A]): ListOps[A] = new ListOps[A] {
    val self = a
  }
}
