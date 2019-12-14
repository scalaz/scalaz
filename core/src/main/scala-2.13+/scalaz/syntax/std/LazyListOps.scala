package scalaz
package syntax
package std

import scalaz.std.{lazylist => l}

final class LazyListOps[A](private val self: LazyList[A]) extends AnyVal {
  final def interleave(other: LazyList[A]): LazyList[A] = l.interleave(self, other)
  final def toZipper: Maybe[Zipper[A]] = l.toZipper(self)
  final def zipperEnd: Maybe[Zipper[A]] = l.zipperEnd(self)
  final def heads: LazyList[LazyList[A]] = l.heads(self)
  final def tails: LazyList[LazyList[A]] = l.tails(self)
  final def zapp[B, C](f: LazyList[A => B => C]): LazyList[B => C] = l.zapp(self)(f)
  final def unfoldForest[B](f: A => (B, () => LazyList[A])): LazyList[Tree[B]] = l.unfoldForest(self)(f)
  final def unfoldForestM[B, M[_] : Monad](f: A => M[(B, LazyList[A])]): M[LazyList[Tree[B]]] = l.unfoldForestM(self)(f)
  final def intersperse(a: A): LazyList[A] = l.intersperse(self, a)
}

trait ToLazyListOps {
  implicit def ToLazyListOpsFromLazyList[A](a: LazyList[A]): LazyListOps[A] = new LazyListOps(a)
}
