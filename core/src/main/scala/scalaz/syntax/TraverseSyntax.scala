package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Traverse` */
trait TraverseV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: Traverse[F]
  ////

  import Leibniz.===
  import Ident.{id}
  import State.State
  import State.state

  final def tmap[B](f: A => B) =
    F.map(self)(f)

  final def traverse[G[_], B](f: A => G[B])(implicit G: Applicative[G]): G[F[B]] =
    G.traverse(self)(f)

  final def traverseI[GB](f: A => GB)(implicit G: Unapply[Applicative, GB]): G.M[F[G.A]] /*G[F[B]*/ = {
    G.TC.traverse(self)(a => G(f(a)))
  }

  final def sequence[G[_], B](implicit ev: A === G[B], G: Applicative[G]): G[F[B]] = {
    val fgb: F[G[B]] = ev.subst[F](self)
    F.sequence(fgb)
  }

  final def traverseS[S, B](f: A => State[S, B]) =
    F.traverseS[S, A, B](self)(f)

  final def runTraverseS[S, B](s: S)(f: A => State[S, B]) =
    F.runTraverseS(self, s)(f)

  final def reverse: F[A] = F.reverse(self)

  final def zipWith[B, C](fb: F[B])(f: (A, Option[B]) => C): (F[C], List[B]) = F.zipWith(self, fb)(f)
  final def zipWithL[B, C](fb: F[B])(f: (A, Option[B]) => C): F[C] = F.zipWithL(self, fb)(f)
  final def zipWithR[B, C](fb: F[B])(f: (Option[A], B) => C): F[C] = F.zipWithR(self, fb)(f)
  final def zipL[B](fb: F[B]): F[(A, Option[B])] = F.zipL(self, fb)
  final def zipR[B](fb: F[B]): F[(Option[A], B)] = F.zipR(self, fb)

  ////
}

trait ToTraverseV extends ToFunctorV with ToFoldableV {
  implicit def ToTraverseV[FA](v: FA)(implicit F0: Unapply[Traverse, FA]) =
    new TraverseV[F0.M,F0.A] { def self = F0(v); implicit def F: Traverse[F0.M] = F0.TC }

  ////

  ////
}

trait TraverseSyntax[F[_]] extends FunctorSyntax[F] with FoldableSyntax[F] {
  implicit def ToTraverseV[A](v: F[A])(implicit F0: Traverse[F]): TraverseV[F, A] = new TraverseV[F,A] { def self = v; implicit def F: Traverse[F] = F0 }

  ////

  ////
}
