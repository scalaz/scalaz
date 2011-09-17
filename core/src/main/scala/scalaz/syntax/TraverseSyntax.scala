package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Traverse` */
trait TraverseV[F[_],A] extends SyntaxV[F[A]] {
  ////
  import Id.{id}
  import State.State
  import State.state

  def tmap[B](f: A => B)(implicit F: Traverse[F]) = F.map(self)(f)

  def traverse[G[_],S,B](f: A => G[B])(implicit F: Traverse[F], G: Applicative[G]) =
    F.traverse(self)(f)

  def traverseS[S,B](f: A => State[S,B])(implicit F: Traverse[F]) =
    F.traverseS[S,A,B](self)(f)

  def runTraverseS[S,B](s: S)(f: A => State[S,B])(implicit F: Traverse[F]) =
    F.runTraverseS(self, s)(f)

  ////
}

trait ToTraverseSyntax extends ToFunctorSyntax {
  implicit def ToTraverseV[F[_],A](v: F[A]) =
    (new TraverseSyntax[F] {}).ToTraverseV(v)
  implicit def ToTraverseVFromBin[F[_, _], X, A](v: F[X, A]) =
    (new TraverseSyntax[({type f[a] = F[X, a]})#f] {}).ToTraverseV(v)
  implicit def ToTraverseVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A]) =
    (new TraverseSyntax[({type f[a] = F[X, G, a]})#f] {}).ToTraverseV(v)
  implicit def ToTraverseVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A]) =
    (new TraverseSyntax[({type f[a] = F[X, Id, a]})#f] {}).ToTraverseV(v)

  ////

  ////
}

trait TraverseSyntax[F[_]] extends FunctorSyntax[F] {
  implicit def ToTraverseV[A](v: F[A]): TraverseV[F, A] = new TraverseV[F,A] { def self = v }

  ////

  ////
}
