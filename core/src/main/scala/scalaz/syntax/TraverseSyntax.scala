package scalaz
package syntax

import Id.{Id,id}
import State.State
import State.state

trait TraverseV[F[_],A] extends SyntaxV[F[A]] {
  def self: F[A]
  def tmap[B](f: A => B)(implicit F: Traverse[F]) = F.map(self)(f)

  def traverse[G[_],S,B](f: A => G[B])(implicit F: Traverse[F], G: Applicative[G]) =
    F.traverse(self)(f)

  def traverseS[S,B](f: A => State[S,B])(implicit F: Traverse[F]) =
    F.traverseS[S,A,B](self)(f)

  def runTraverseS[S,B](s: S)(f: A => State[S,B])(implicit F: Traverse[F]) =
    F.runTraverseS(self, s)(f)
}

trait ToTraverseSyntax {
  implicit def traverse[F[_],A](v: F[A]) = (new TraverseSyntax[F] {}).traverseV(v)
}
trait TraverseSyntax[F[_]] {
  implicit def traverseV[A](v: F[A]) = new TraverseV[F,A] { def self = v }
}
