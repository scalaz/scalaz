package scalaz
package syntax
package std

import scalaz.std.{vector => v}


final class VectorOps[A](private val self: Vector[A]) extends AnyVal {
  final def intersperse(a: A): Vector[A] = v.intersperse(self, a)

  final def toNel: Option[NonEmptyList[A]] = v.toNel(self)

  final def toZipper: Option[Zipper[A]] = v.toZipper(self)

  final def zipperEnd: Option[Zipper[A]] = v.zipperEnd(self)

  final def <^>[B: Monoid](f: NonEmptyList[A] => B): B = v.<^>(self)(f)

  final def takeWhileM[M[_] : Monad](p: A => M[Boolean]): M[Vector[A]] = v.takeWhileM(self)(p)

  final def takeUntilM[M[_] : Monad](p: A => M[Boolean]): M[Vector[A]] = v.takeUntilM(self)(p)

  final def filterM[M[_] : Applicative](p: A => M[Boolean]): M[Vector[A]] = v.filterM(self)(p)

  final def findM[M[_] : Monad](p: A => M[Boolean]): M[Option[A]] = v.findM(self)(p)

  final def powerset: Vector[Vector[A]] = v.powerset(self)

  final def partitionM[M[_] : Applicative](p: A => M[Boolean]): M[(Vector[A], Vector[A])] = v.partitionM(self)(p)

  final def spanM[M[_] : Monad](p: A => M[Boolean]): M[(Vector[A], Vector[A])] = v.spanM(self)(p)

  final def breakM[M[_] : Monad](p: A => M[Boolean]): M[(Vector[A], Vector[A])] = v.breakM(self)(p)

  final def groupWhenM[M[_] : Monad](p: (A, A) => M[Boolean]): M[Vector[Vector[A]]] = v.groupWhenM(self)(p)

  final def groupWhen(p: (A, A) => Boolean): Vector[Vector[A]] = v.groupWhen(self)(p)

  final def mapAccumLeft[B, C](c: C, f: (C, A) => (C, B)): (C, Vector[B]) = v.mapAccumLeft(self)(c, f)

  final def mapAccumRight[B, C](c: C, f: (C, A) => (C, B)): (C, Vector[B]) = v.mapAccumRight(self)(c, f)

  final def tailz: Vector[Vector[A]] = v.tailz(self)

  final def initz: Vector[Vector[A]] = v.initz(self)

  final def allPairs: Vector[(A, A)] = v.allPairs(self)

  final def adjacentPairs: Vector[(A, A)] = v.adjacentPairs(self)
}

trait ToVectorOps {
  implicit def ToVectorOpsFromVector[A](a: Vector[A]): VectorOps[A] = new VectorOps[A](a)
}
