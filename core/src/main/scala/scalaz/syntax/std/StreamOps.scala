package scalaz
package syntax
package std

import scalaz.syntax.Ops
import scalaz.{Monad, Tree, Zipper}
import scalaz.std.{stream => s}


trait StreamOps[A] extends Ops[Stream[A]] {
  final def merge(other: Stream[A]): Stream[A] = s.merge(self, other)
  final def toZipper: Option[Zipper[A]] = s.toZipper(self)
  final def zipperEnd: Option[Zipper[A]] = s.zipperEnd(self)
  final def heads: Stream[Stream[A]] = s.heads(self)
  final def tails: Stream[Stream[A]] = s.tails(self)
  final def zapp[B, C](f: Stream[A => B => C]): Stream[(B) => C] = s.zapp(self)(f)
  final def unfoldForest[B](f: A => (B, () => Stream[A])): Stream[Tree[B]] = s.unfoldForest(self)(f)
  final def unfoldForestM[B, M[_] : Monad](f: A => M[(B, Stream[A])]): M[Stream[Tree[B]]] = s.unfoldForestM(self)(f)
  final def intersperse(a: A): Stream[A] = s.intersperse(self, a)
  final def intercalate[B](bs: Stream[B])(implicit ev: Stream[A] =:= Stream[Stream[B]]): Stream[B] = s.intercalate(self, bs)
}

trait ToStreamOps {
  implicit def ToStreamOpsFromStream[A](a: Stream[A]): StreamOps[A] = new StreamOps[A] {
    val self = a
  }
}
