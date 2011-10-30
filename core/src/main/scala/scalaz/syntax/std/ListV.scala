package scalaz
package syntax
package std

import scalaz.std.list


trait ListV[A] extends SyntaxV[List[A]] {

  final def intersperse(a: A): List[A] = list.intersperse(self, a)

  final def intercalate(other: List[A]): List[A] = list.intercalate(self, other)

  final def toNel: Option[NonEmptyList[A]] = list.toNel(self)

  final def toZipper: Option[Zipper[A]] = list.toZipper(self)

  final def zipperEnd: Option[Zipper[A]] = list.zipperEnd(self)

  final def <^>[B: Monoid](f: NonEmptyList[A] => B): B = list.<^>(self)(f)

  final def takeWhileM[M[_] : Monad](p: A => M[Boolean]): M[List[A]] = list.takeWhileM(self)(p)

  final def takeUntilM[M[_] : Monad](p: A => M[Boolean]): M[List[A]] = list.takeUntilM(self)(p)

  final def filterM[M[_] : Monad](p: A => M[Boolean]): M[List[A]] = list.filterM(self)(p)

  final def findM[M[_] : Monad](p: A => M[Boolean]): M[Option[A]] = list.findM(self)(p)

  final def powerset: List[List[A]] = list.powerset(self)

  final def partitionM[M[_] : Monad](p: A => M[Boolean]): M[(List[A], List[A])] = list.partitionM(self)(p)

  final def spanM[M[_] : Monad](p: A => M[Boolean]): M[(List[A], List[A])] = list.spanM(self)(p)

  final def breakM[M[_] : Monad](p: A => M[Boolean]): M[(List[A], List[A])] = list.breakM(self)(p)

  final def groupByM[M[_] : Monad](p: (A, A) => M[Boolean]): M[List[List[A]]] = list.groupByM(self)(p)

  final def mapAccumLeft[B, C](c: C, f: (C, A) => (C, B)): (C, List[B]) = list.mapAccumLeft(self)(c, f)

  final def mapAccumRight[B, C](c: C, f: (C, A) => (C, B)): (C, List[B]) = list.mapAccumRight(self)(c, f)

  final def tailz: List[List[A]] = list.tailz(self)

  final def initz: List[List[A]] = list.initz(self)

  final def allPairs: List[(A, A)] = list.allPairs(self)

  final def adjacentPairs: List[(A, A)] = list.adjacentPairs(self)
}

trait ToListV {
  implicit def ToListVFromList[A](a: List[A]): ListV[A] = new ListV[A] {
    val self = a
  }
}
