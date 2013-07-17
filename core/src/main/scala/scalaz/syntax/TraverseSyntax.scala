package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Traverse` */
sealed abstract class TraverseOps[F[_],A] extends Ops[F[A]] {
  implicit def F: Traverse[F]
  ////

  import Leibniz.===

  final def tmap[B](f: A => B) =
    F.map(self)(f)

  final def traverse[G[_], B](f: A => G[B])(implicit G: Applicative[G]): G[F[B]] =
    G.traverse(self)(f)

  /** A version of `traverse` that infers the type constructor `G` */
  final def traverseU[GB](f: A => GB)(implicit G: Unapply[Applicative, GB]): G.M[F[G.A]] /*G[F[B]]*/ =
    F.traverseU[A, GB](self)(f)(G)

  /** A version of `traverse` where a subsequent monadic join is applied to the inner result. */
  final def traverseM[G[_], B](f: A => G[F[B]])(implicit G: Applicative[G], FM: Monad[F]): G[F[B]] =
    F.traverseM[A, G, B](self)(f)(G, FM)

  /** Traverse with the identity function */
  final def sequence[G[_], B](implicit ev: A === G[B], G: Applicative[G]): G[F[B]] = {
    val fgb: F[G[B]] = ev.subst[F](self)
    F.sequence(fgb)
  }

  /** A version of `sequence` that infers the nested type constructor */
  final def sequenceU(implicit G: Unapply[Applicative, A]): G.M[F[G.A]] /*G[F[A]] */ = {
    G.TC.traverse(self)(x => G.apply(x))
  }

  /** A version of `traverse` specialized for `State` */
  final def traverseS[S, B](f: A => State[S, B]): State[S, F[B]] =
    F.traverseS[S, A, B](self)(f)

  /**
   * A version of `traverse` specialized for `State[S, G[B]]` that internally uses a `Trampoline`
   * to avoid stack-overflow.
   */
  final def traverseSTrampoline[G[_]: Applicative, S, B](f: A => State[S, G[B]]): State[S, G[F[B]]] =
    F.traverseSTrampoline[S, G, A, B](self)(f)

  /**
   * A version of `traverse` specialized for `Kleisli[G, S, B]` that internally uses a `Trampoline`
   * to avoid stack-overflow.
   */
  final def traverseKTrampoline[G[_]: Applicative, S, B](f: A => Kleisli[G, S, B]): Kleisli[G, S, F[B]] =
    F.traverseKTrampoline[S, G, A, B](self)(f)

  final def runTraverseS[S, B](s: S)(f: A => State[S, B]): (S, F[B]) =
    F.runTraverseS(self, s)(f)

  final def reverse: F[A] = F.reverse(self)

  final def zipWith[B, C](fb: F[B])(f: (A, Option[B]) => C): (List[B], F[C]) = F.zipWith(self, fb)(f)
  final def zipWithL[B, C](fb: F[B])(f: (A, Option[B]) => C): F[C] = F.zipWithL(self, fb)(f)
  final def zipWithR[B, C](fb: F[B])(f: (Option[A], B) => C): F[C] = F.zipWithR(self, fb)(f)
  final def zipL[B](fb: F[B]): F[(A, Option[B])] = F.zipL(self, fb)
  final def zipR[B](fb: F[B]): F[(Option[A], B)] = F.zipR(self, fb)
  final def mapAccumL[S,B](z: S)(f: (S,A) => (S,B)): (S, F[B]) =
    F.mapAccumL(self, z)(f)
  final def mapAccumR[S,B](z: S)(f: (S,A) => (S,B)): (S, F[B]) =
    F.mapAccumR(self, z)(f)
  ////
}

trait ToTraverseOps0 {
  implicit def ToTraverseOpsUnapply[FA](v: FA)(implicit F0: Unapply[Traverse, FA]) =
    new TraverseOps[F0.M,F0.A] { def self = F0(v); implicit def F: Traverse[F0.M] = F0.TC }

}

trait ToTraverseOps extends ToTraverseOps0 with ToFunctorOps with ToFoldableOps {
  implicit def ToTraverseOps[F[_],A](v: F[A])(implicit F0: Traverse[F]) =
    new TraverseOps[F,A] { def self = v; implicit def F: Traverse[F] = F0 }

  ////

  ////
}

trait TraverseSyntax[F[_]] extends FunctorSyntax[F] with FoldableSyntax[F] {
  implicit def ToTraverseOps[A](v: F[A]): TraverseOps[F, A] = new TraverseOps[F,A] { def self = v; implicit def F: Traverse[F] = TraverseSyntax.this.F }

  def F: Traverse[F]
  ////

  ////
}
