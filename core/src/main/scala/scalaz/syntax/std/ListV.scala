package scalaz
package syntax
package std

import scalaz.std.list


trait ListV[A] extends SyntaxV[List[A]] {

  def intersperse(a: A): List[A] = list.intersperse(self, a)

  def intercalate(other: List[A]): List[A] = list.intercalate(self, other)

  def toNel: Option[NonEmptyList[A]] = list.toNel(self)

  def toZipper: Option[Zipper[A]] = list.toZipper(self)

  def zipperEnd: Option[Zipper[A]] = list.zipperEnd(self)

  def <^>[B: Monoid](f: NonEmptyList[A] => B): B = list.<^>(self)(f)

  def takeWhileM[M[_] : Monad](p: A => M[Boolean]): M[List[A]] = list.takeWhileM(self)(p)

  def takeUntilM[M[_] : Monad](p: A => M[Boolean]): M[List[A]] = list.takeUntilM(self)(p)

  def filterM[M[_] : Monad](p: A => M[Boolean]): M[List[A]] = list.filterM(self)(p)

  def findM[M[_] : Monad](p: A => M[Boolean]): M[Option[A]] = list.findM(self)(p)

  def powerset: List[List[A]] = list.powerset(self)

  def partitionM[M[_] : Monad](p: A => M[Boolean]): M[(List[A], List[A])] = list.partitionM(self)(p)

  def spanM[M[_] : Monad](p: A => M[Boolean]): M[(List[A], List[A])] = list.spanM(self)(p)

  def breakM[M[_] : Monad](p: A => M[Boolean]): M[(List[A], List[A])] = list.breakM(self)(p)

  def groupByM[M[_] : Monad](p: (A, A) => M[Boolean]): M[List[List[A]]] = list.groupByM(self)(p)

  def mapAccumLeft[B, C](c: C, f: (C, A) => (C, B)): (C, List[B]) = list.mapAccumLeft(self)(c, f)

  def mapAccumRight[B, C](c: C, f: (C, A) => (C, B)): (C, List[B]) = list.mapAccumRight(self)(c, f)

  def tailz: List[List[A]] = list.tailz(self)

  def initz: List[List[A]] = list.initz(self)

  def allPairs: List[(A, A)] = list.allPairs(self)

  def adjacentPairs: List[(A, A)] = list.adjacentPairs(self)
}

trait ToListV {
  implicit def ToListVFromList[A](a: List[A]): ListV[A] = new ListV[A] {
    val self = a
  }
}
