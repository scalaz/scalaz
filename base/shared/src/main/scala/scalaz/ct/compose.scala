package scalaz
package ct

import scala.{ Option, Some }

sealed abstract class ComposeModule {
  type Compose[F[_], G[_], X]

  def apply[F[_], G[_], X](fgx: F[G[X]]): Compose[F, G, X]
  def run[F[_], G[_], X](compo: Compose[F, G, X]): F[G[X]]

  final def unapply[F[_], G[_], X](arg: Compose[F, G, X]): Option[F[G[X]]] =
    Some(run(arg))

  // typeclass instances

  def invariant[F[_]: InvariantFunctor, G[_]: InvariantFunctor]: InvariantFunctor[Compose[F, G, ?]]

  def functor[F[_]: Functor, G[_]: Functor]: Functor[Compose[F, G, ?]]
  def apply[F[_]: Apply, G[_]: Apply]: Apply[Compose[F, G, ?]]
  def applicative[F[_]: Applicative, G[_]: Applicative]: Applicative[Compose[F, G, ?]]

  def contravariant1[F[_]: Contravariant, G[_]: Functor]: Contravariant[Compose[F, G, ?]]
  def contravariant2[F[_]: Functor, G[_]: Contravariant]: Contravariant[Compose[F, G, ?]]

  def foldable[F[_]: Foldable, G[_]: Foldable]: Foldable[Compose[F, G, ?]]
  def traversable[F[_]: Traversable, G[_]: Traversable]: Traversable[Compose[F, G, ?]]
}

object ComposeModule {
  implicit def invariant[F[_]: InvariantFunctor, G[_]: InvariantFunctor]: InvariantFunctor[Compose[F, G, ?]] =
    Compose.invariant

  implicit def functor[F[_]: Functor, G[_]: Functor]: Functor[Compose[F, G, ?]]                 = Compose.functor
  implicit def apply[F[_]: Apply, G[_]: Apply]: Apply[Compose[F, G, ?]]                         = Compose.apply
  implicit def applicative[F[_]: Applicative, G[_]: Applicative]: Applicative[Compose[F, G, ?]] = Compose.applicative

  implicit def contravariant1[F[_]: Contravariant, G[_]: Functor]: Contravariant[Compose[F, G, ?]] =
    Compose.contravariant1

  implicit def foldable[F[_]: Foldable, G[_]: Foldable]: Foldable[Compose[F, G, ?]]             = Compose.foldable
  implicit def traversable[F[_]: Traversable, G[_]: Traversable]: Traversable[Compose[F, G, ?]] = Compose.traversable
}

private[ct] sealed abstract class ComposeModule0 { this: ComposeModule.type =>
  implicit def contravariant2[F[_]: Functor, G[_]: Contravariant]: Contravariant[Compose[F, G, ?]] =
    Compose.contravariant2
}

private[ct] object ComposeImpl extends ComposeModule {
  type Compose[F[_], G[_], X] = F[G[X]]

  def apply[F[_], G[_], X](fgx: F[G[X]]): Compose[F, G, X] = fgx
  def run[F[_], G[_], X](compo: Compose[F, G, X]): F[G[X]] = compo

  def invariant[F[_], G[_]](implicit F0: InvariantFunctor[F],
                            G0: InvariantFunctor[G]): InvariantFunctor[Compose[F, G, ?]] =
    instanceOf(new ComposeInvariantFunctor[F, G] {
      val F = F0
      val G = G0
    })

  def functor[F[_], G[_]](implicit F0: Functor[F], G0: Functor[G]): Functor[Compose[F, G, ?]] =
    instanceOf(new ComposeFunctor[F, G] {
      val F = F0
      val G = G0
    })

  def apply[F[_], G[_]](implicit F0: Apply[F], G0: Apply[G]): Apply[Compose[F, G, ?]] =
    instanceOf(new ComposeApply[F, G] {
      val F = F0
      val G = G0
    })

  def applicative[F[_], G[_]](implicit F0: Applicative[F], G0: Applicative[G]): Applicative[Compose[F, G, ?]] =
    instanceOf(new ComposeApplicative[F, G] {
      val F = F0
      val G = G0
    })

  def contravariant1[F[_], G[_]](implicit F0: Contravariant[F], G0: Functor[G]): Contravariant[Compose[F, G, ?]] =
    instanceOf(new ComposeContravariant1[F, G] {
      val F = F0
      val G = G0
    })

  def contravariant2[F[_], G[_]](implicit F0: Functor[F], G0: Contravariant[G]): Contravariant[Compose[F, G, ?]] =
    instanceOf(new ComposeContravariant2[F, G] {
      val F = F0
      val G = G0
    })

  def foldable[F[_], G[_]](implicit F0: Foldable[F], G0: Foldable[G]): Foldable[Compose[F, G, ?]] =
    instanceOf(new ComposeFoldable[F, G] {
      val F = F0
      val G = G0
    })

  def traversable[F[_], G[_]](implicit F0: Traversable[F], G0: Traversable[G]): Traversable[Compose[F, G, ?]] =
    instanceOf(new ComposeTraversable[F, G] {
      val F = F0
      val G = G0
    })

  private trait ComposeInvariantFunctor[F[_], G[_]] extends InvariantFunctorClass[Compose[F, G, ?]] {
    val F: InvariantFunctorClass[F]
    val G: InvariantFunctorClass[G]

    final def imap[A, B](ma: F[G[A]])(f: A => B)(g: B => A): F[G[B]] =
      F.imap[G[A], G[B]](ma)(G.imap(_)(f)(g))(G.imap(_)(g)(f))
  }

  private trait ComposeFunctor[F[_], G[_]] extends FunctorClass[Compose[F, G, ?]] {
    val F: FunctorClass[F]
    val G: FunctorClass[G]

    final def map[A, B](fa: F[G[A]])(f: A => B): F[G[B]] =
      F.map(fa)(G.map(_)(f))
  }

  private trait ComposeApply[F[_], G[_]] extends ComposeFunctor[F, G] with ApplyClass[Compose[F, G, ?]] {
    val F: ApplyClass[F]
    val G: ApplyClass[G]

    final def ap[A, B](fa: F[G[A]])(f: F[G[A => B]]): F[G[B]] =
      F.ap(fa)(F.map(f)(gab => G.ap(_)(gab)))
  }

  private trait ComposeApplicative[F[_], G[_]] extends ComposeApply[F, G] with ApplicativeClass[Compose[F, G, ?]] {
    val F: ApplicativeClass[F]
    val G: ApplicativeClass[G]

    final def pure[A](a: A): F[G[A]] = F.pure(G.pure(a))
  }

  private trait ComposeContravariant1[F[_], G[_]] extends ContravariantClass[Compose[F, G, ?]] {
    val F: ContravariantClass[F]
    val G: FunctorClass[G]

    final def contramap[A, B](r: F[G[A]])(f: B => A): F[G[B]] =
      F.contramap(r)(G.map(_)(f))
  }

  private trait ComposeContravariant2[F[_], G[_]] extends ContravariantClass[Compose[F, G, ?]] {
    val F: FunctorClass[F]
    val G: ContravariantClass[G]

    final def contramap[A, B](r: F[G[A]])(f: B => A): F[G[B]] =
      F.map(r)(G.contramap(_)(f))
  }

  private trait ComposeFoldable[F[_], G[_]] extends FoldableClass[Compose[F, G, ?]] {
    val F: FoldableClass[F]
    val G: FoldableClass[G]

    def foldMap[A, B](fa: F[G[A]])(f: A => B)(implicit B: Monoid[B]): B =
      F.foldMap(fa)(G.foldMap(_)(f))

    def foldRight[A, B](fa: F[G[A]], z: => B)(f: (A, => B) => B): B =
      F.foldRight(fa, z)((ga, b) => G.foldRight(ga, b)(f))

    def foldLeft[A, B](fa: F[G[A]], z: B)(f: (B, A) => B): B =
      F.foldLeft(fa, z)((b, ga) => G.foldLeft(ga, b)(f))

    def toList[A](fa: F[G[A]]): scala.List[A] =
      F.toList(fa).flatMap(G.toList)
  }

  private trait ComposeTraversable[F[_], G[_]]
      extends ComposeFunctor[F, G]
      with ComposeFoldable[F, G]
      with TraversableClass[Compose[F, G, ?]] {
    val F: TraversableClass[F]
    val G: TraversableClass[G]

    def sequence[Z[_], A](ta: F[G[Z[A]]])(implicit Z: Applicative[Z]): Z[F[G[A]]] =
      F.traverse(ta)(G.sequence(_))

    def traverse[Z[_], A, B](ta: F[G[A]])(f: A => Z[B])(implicit Z: Applicative[Z]): Z[F[G[B]]] =
      F.traverse(ta)(G.traverse(_)(f))
  }

}
