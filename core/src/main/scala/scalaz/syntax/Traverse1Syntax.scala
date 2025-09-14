package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Traverse1` */
final class Traverse1Ops[F[_],A] private[syntax](val self: F[A])(implicit val F: Traverse1[F]) extends Ops[F[A]] {
  ////


  final def traverse1[G[_], B](f: A => G[B])(implicit G: Apply[G]): G[F[B]] =
    G.traverse1(self)(f)

  /** A version of `traverse1` that infers the type constructor `G` */
  final def traverse1U[GB](f: A => GB)(implicit G: Unapply[Apply, GB]): G.M[F[G.A]] =
    F.traverse1U[A, GB](self)(f)(using G)

  final def traverse1M[G[_], B](f: A => G[F[B]])(implicit G: Apply[G], FM: Bind[F]): G[F[B]] =
    F.traverse1M[A, G, B](self)(f)(using G, FM)

  /** Traverse with the identity function */
  final def sequence1[G[_], B](implicit ev: A === G[B], G: Apply[G]): G[F[B]] = {
    val fgb: F[G[B]] = ev.subst[F](self)
    F.sequence1(fgb)
  }

  final def sequence1M[G[_], B](implicit ev: A === G[F[B]], G: Apply[G], FM: Bind[F]): G[F[B]] = {
    val fgfb: F[G[F[B]]] = ev.subst[F](self)
    F.sequence1M[B, G](fgfb)
  }

  /** A version of `sequence1` that infers the nested type constructor */
  final def sequence1U(implicit G: Unapply[Apply, A]): G.M[F[G.A]] =
    F.sequence1U(self)

  ////
}

sealed trait ToTraverse1OpsU[TC[F[_]] <: Traverse1[F]] {
  implicit def ToTraverse1OpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]): Traverse1Ops[F0.M, F0.A] =
    new Traverse1Ops[F0.M, F0.A](F0(v))(using F0.TC)

}

trait ToTraverse1Ops0[TC[F[_]] <: Traverse1[F]] extends ToTraverse1OpsU[TC] {
  implicit def ToTraverse1Ops[F[_],A](v: F[A])(implicit F0: TC[F]): Traverse1Ops[F, A] =
    new Traverse1Ops[F, A](v)

  ////

  ////
}

trait ToTraverse1Ops[TC[F[_]] <: Traverse1[F]] extends ToTraverse1Ops0[TC] with ToTraverseOps[TC] with ToFoldable1Ops[TC]

trait Traverse1Syntax[F[_]] extends TraverseSyntax[F] with Foldable1Syntax[F] {
  implicit def ToTraverse1Ops[A](v: F[A]): Traverse1Ops[F, A] = new Traverse1Ops[F,A](v)(using Traverse1Syntax.this.F)

  def F: Traverse1[F]
  ////

  ////
}
