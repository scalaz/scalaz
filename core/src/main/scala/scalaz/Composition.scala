package scalaz

private[scalaz] trait CompositionFunctor[F[_], G[_]] extends Functor[({type λ[α] = F[G[α]]})#λ] {
  implicit def F: Functor[F]

  implicit def G: Functor[G]

  override def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] = F(fga)(ga => G(ga)(f))
}

private[scalaz] trait CompositionApply[F[_], G[_]] extends Apply[({type λ[α] = F[G[α]]})#λ] with CompositionFunctor[F, G] {
  implicit def F: Apply[F]

  implicit def G: Apply[G]

  def ap[A, B](fa: => F[G[A]])(f: => F[G[A => B]]): F[G[B]] =
    F.apply2(f, fa)((ff, ga) => G.ap(ga)(ff))
}

private[scalaz] trait CompositionApplicative[F[_], G[_]] extends Applicative[({type λ[α] = F[G[α]]})#λ] with CompositionApply[F, G] {
  implicit def F: Applicative[F]

  implicit def G: Applicative[G]

  def point[A](a: => A): F[G[A]] = F.point(G.point(a))
}

private[scalaz] trait CompositionPlus[F[_], G[_]] extends Plus[({type λ[α] = F[G[α]]})#λ] {
  implicit def F: Plus[F]

  implicit def G: Plus[G]

  def plus[A](a: F[G[A]], b: => F[G[A]]): F[G[A]] =
    F.plus(a, b)
}

private[scalaz] trait CompositionPlusEmpty[F[_], G[_]] extends PlusEmpty[({type λ[α] = F[G[α]]})#λ] with CompositionPlus[F, G] {
  implicit def F: PlusEmpty[F]

  implicit def G: PlusEmpty[G]

  def empty[A]: F[G[A]] = F.empty[G[A]]
}

private[scalaz] trait CompositionApplicativePlus[F[_], G[_]] extends ApplicativePlus[({type λ[α] = F[G[α]]})#λ] with CompositionApplicative[F, G] with CompositionPlusEmpty[F, G] {
  implicit def F: ApplicativePlus[F]

  implicit def G: ApplicativePlus[G]
}

private[scalaz] trait CompositionFoldable[F[_], G[_]] extends Foldable[({type λ[α] = F[G[α]]})#λ]  {
  implicit def F: Foldable[F]

  implicit def G: Foldable[G]

  override def foldRight[A, B](fa: F[G[A]], z: => B)(f: (A, => B) => B): B =
    F.foldRight(fa, z)((a, b) => G.foldRight(a, b)(f))

  override def foldMap[A,B](fa: F[G[A]])(f: A => B)(implicit M: Monoid[B]): B =
    F.foldMap(fa)(G.foldMap(_)(f))

  override def foldLeft[A, B](fa: F[G[A]], z: B)(f: (B, A) => B): B =
    F.foldLeft(fa, z)((b, a) => G.foldLeft(a, b)(f))
}

private[scalaz] trait CompositionTraverse[F[_], G[_]] extends Traverse[({type λ[α] = F[G[α]]})#λ] with CompositionFunctor[F, G] with CompositionFoldable[F, G] {
  implicit def F: Traverse[F]

  implicit def G: Traverse[G]

  def traverseImpl[X[_]:Applicative, A, B](a: F[G[A]])(f: A => X[B]): X[F[G[B]]] =
    F.traverse(a)(G.traverse(_)(f))

}

private[scalaz] trait CompositionDistributive[F[_], G[_]] extends Distributive[({type λ[α] = F[G[α]]})#λ] with CompositionFunctor[F, G] {
  implicit def F: Distributive[F]

  implicit def G: Distributive[G]

  def distributeImpl[X[_]:Functor, A, B](a: X[A])(f: A => F[G[B]]): F[G[X[B]]] =
    F(F.distribute(a)(f))(G.cosequence(_))
}

private[scalaz] trait CompositionZip[F[_], G[_]] extends Zip[({type λ[α] = F[G[α]]})#λ] {
  implicit def T: Functor[F]

  implicit def F: Zip[F]

  implicit def G: Zip[G]

  def zip[A, B](a: => F[G[A]], b: => F[G[B]]): F[G[(A, B)]] =
    F.zipWith(a, b)(G.zip(_, _))
}

private[scalaz] trait CompositionUnzip[F[_], G[_]] extends Unzip[({type λ[α] = F[G[α]]})#λ] {
  implicit def T: Functor[F]

  implicit def F: Unzip[F]

  implicit def G: Unzip[G]

  def unzip[A, B](a: F[G[(A, B)]]): (F[G[A]], F[G[B]]) = {
    val f = T.map(a)(G.firsts(_))
    val g = T.map(a)(G.seconds(_))
    (f, g)
  }
}

private[scalaz] trait CompositionBifunctor[F[_, _], G[_, _]] extends Bifunctor[({type λ[α, β]=F[G[α, β], G[α, β]]})#λ] {
  implicit def F: Bifunctor[F]

  implicit def G: Bifunctor[G]

  override def bimap[A, B, C, D](fab: F[G[A, B], G[A, B]])(f: A => C, g: B => D): F[G[C, D], G[C, D]] =
    F.bimap(fab)(G.bimap(_)(f, g), G.bimap(_)(f, g))
}

private[scalaz] trait CompositionBifoldable[F[_, _], G[_, _]] extends Bifoldable[({type λ[α, β]=F[G[α, β], G[α, β]]})#λ] {
  implicit def F: Bifoldable[F]

  implicit def G: Bifoldable[G]

  override def bifoldMap[A,B,M](fa: F[G[A, B], G[A, B]])(f: A => M)(g: B => M)(implicit M: Monoid[M]): M =
    F.bifoldMap(fa)(G.bifoldMap(_)(f)(g))(G.bifoldMap(_)(f)(g))
  override def bifoldRight[A,B,C](fa: F[G[A, B], G[A, B]], z: => C)(f: (A, => C) => C)(g: (B, => C) => C): C =
    F.bifoldRight(fa, z)((a, b) => G.bifoldRight(a, b)(f)(g))((a, b) => G.bifoldRight(a, b)(f)(g))
  override def bifoldLeft[A,B,C](fa: F[G[A, B], G[A, B]], z: C)(f: (C, A) => C)(g: (C, B) => C): C =
    F.bifoldLeft(fa, z)((b, a) => G.bifoldLeft(a, b)(f)(g))((b, a) => G.bifoldLeft(a, b)(f)(g))
}

private[scalaz] trait CompositionBitraverse[F[_, _], G[_, _]]
  extends Bitraverse[({type λ[α, β]=F[G[α, β], G[α, β]]})#λ]
  with CompositionBifunctor[F, G] with CompositionBifoldable[F, G]{

  implicit def F: Bitraverse[F]

  implicit def G: Bitraverse[G]

  def bitraverseImpl[X[_] : Applicative, A, B, C, D](fab: F[G[A, B], G[A, B]])(f: A => X[C], g: B => X[D]): X[F[G[C, D], G[C, D]]] =
    F.bitraverseImpl(fab)(G.bitraverse(_)(f)(g), G.bitraverse(_)(f)(g))

}

