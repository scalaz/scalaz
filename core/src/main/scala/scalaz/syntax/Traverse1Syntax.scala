package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Traverse1` */
sealed abstract class Traverse1Ops[F[_],A] extends Ops[F[A]] {
  implicit def F: Traverse1[F]
  ////

  import Leibniz.===

  final def traverse1[G[_], B](f: A => G[B])(implicit G: Apply[G]): G[F[B]] =
    G.traverse1(self)(f)

  /** Traverse with the identity function */
  final def sequence1[G[_], B](implicit ev: A === G[B], G: Apply[G]): G[F[B]] = {
    val fgb: F[G[B]] = ev.subst[F](self)
    F.sequence1(fgb)
  }
  ////
}

trait ToTraverse1Ops0 {
  implicit def ToTraverse1OpsUnapply[FA](v: FA)(implicit F0: Unapply[Traverse1, FA]) =
    new Traverse1Ops[F0.M,F0.A] { def self = F0(v); implicit def F: Traverse1[F0.M] = F0.TC }

}

trait ToTraverse1Ops extends ToTraverse1Ops0 with ToTraverseOps with ToFoldable1Ops {
  implicit def ToTraverse1Ops[F[_],A](v: F[A])(implicit F0: Traverse1[F]) =
    new Traverse1Ops[F,A] { def self = v; implicit def F: Traverse1[F] = F0 }

  ////

  ////
}

trait Traverse1Syntax[F[_]] extends TraverseSyntax[F] with Foldable1Syntax[F] {
  implicit def ToTraverse1Ops[A](v: F[A]): Traverse1Ops[F, A] = new Traverse1Ops[F,A] { def self = v; implicit def F: Traverse1[F] = Traverse1Syntax.this.F }

  def F: Traverse1[F]
  ////

  ////
}
