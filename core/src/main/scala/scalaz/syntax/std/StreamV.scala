package scalaz
package syntax
package std

import scalaz.syntax.SyntaxV
import scalaz.{Monad, Tree, Zipper}
import scalaz.std.Stream


trait StreamV[A] extends SyntaxV[Stream[A]] {
  def merge(other: Stream[A]): Stream[A] = Stream.merge(self, other)
  def toZipper: Option[Zipper[A]] = Stream.toZipper(self)
  def zipperEnd: Option[Zipper[A]] = Stream.zipperEnd(self)
  def heads: Stream[Stream[A]] = Stream.heads(self)
  def tails: Stream[Stream[A]] = Stream.tails(self)
  def zapp[B, C](f: Stream[A => B => C]): Stream[(B) => C] = Stream.zapp(self)(f)
  def unfoldForest[B](f: A => (B, () => Stream[A])): Stream[Tree[B]] = Stream.unfoldForest(self)(f)
  def unfoldForestM[B, M[_] : Monad](f: A => M[(B, Stream[A])]): M[Stream[Tree[B]]] = Stream.unfoldForestM(self)(f)
}

trait ToStreamV {
  implicit def ToStreamVFromStream[A](a: Stream[A]): StreamV[A] = new StreamV[A] {
    val self = a
  }
}
