package scalaz
package syntax
package std

import scalaz.syntax.SyntaxV
import scalaz.{Monad, Tree, Zipper}
import scalaz.std.stream


trait StreamV[A] extends SyntaxV[Stream[A]] {
  def merge(other: Stream[A]): Stream[A] = stream.merge(self, other)
  def toZipper: Option[Zipper[A]] = stream.toZipper(self)
  def zipperEnd: Option[Zipper[A]] = stream.zipperEnd(self)
  def heads: Stream[Stream[A]] = stream.heads(self)
  def tails: Stream[Stream[A]] = stream.tails(self)
  def zapp[B, C](f: Stream[A => B => C]): Stream[(B) => C] = stream.zapp(self)(f)
  def unfoldForest[B](f: A => (B, () => Stream[A])): Stream[Tree[B]] = stream.unfoldForest(self)(f)
  def unfoldForestM[B, M[_] : Monad](f: A => M[(B, Stream[A])]): M[Stream[Tree[B]]] = stream.unfoldForestM(self)(f)
}

trait ToStreamV {
  implicit def ToStreamVFromStream[A](a: Stream[A]): StreamV[A] = new StreamV[A] {
    val self = a
  }
}
