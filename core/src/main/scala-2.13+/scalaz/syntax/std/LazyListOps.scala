package scalaz
package syntax
package std

import scalaz.std.{lazylist => l}


final class LazyListOps[A](val self: LazyList[A]) extends AnyVal {
  def interleave(other: LazyList[A]): LazyList[A] = l.interleave(self, other)
  def toZipper: Option[Zipper[A]] = l.toZipper(self)
  def zipperEnd: Option[Zipper[A]] = l.zipperEnd(self)
  def heads: LazyList[LazyList[A]] = l.heads(self)
  def tails: LazyList[LazyList[A]] = l.tails(self)
  def zapp[B, C](f: LazyList[A => B => C]): LazyList[B => C] = l.zapp(self)(f)
  def unfoldForest[B](f: A => (B, () => LazyList[A])): LazyList[Tree[B]] = l.unfoldForest(self)(f)
  def unfoldForestM[B, M[_] : Monad](f: A => M[(B, LazyList[A])]): M[LazyList[Tree[B]]] = l.unfoldForestM(self)(f)
  def intersperse(a: A): LazyList[A] = l.intersperse(self, a)
}

trait ToLazyListOps {
  implicit def ToLazyListOpsFromLazyList[A](a: LazyList[A]): LazyListOps[A] = new LazyListOps(a)
}
