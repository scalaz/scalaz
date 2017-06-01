package scalaz

private trait CompositionFunctor[F[_], G[_]] extends Functor[λ[α => F[G[α]]]] {
  implicit def F: Functor[F]

  implicit def G: Functor[G]

  override def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] = F(fga)(G.lift(f))
}

private trait CompositionApply[F[_], G[_]] extends Apply[λ[α => F[G[α]]]] with CompositionFunctor[F, G] {
  implicit def F: Apply[F]

  implicit def G: Apply[G]

  def ap[A, B](fa: => F[G[A]])(f: => F[G[A => B]]): F[G[B]] =
    F.apply2(f, fa)((ff, ga) => G.ap(ga)(ff))
}

private trait CompositionApplicative[F[_], G[_]] extends Applicative[λ[α => F[G[α]]]] with CompositionApply[F, G] {
  implicit def F: Applicative[F]

  implicit def G: Applicative[G]

  def point[A](a: => A): F[G[A]] = F.point(G.point(a))
}

private trait CompositionPlus[F[_], G[_]] extends Plus[λ[α => F[G[α]]]] {
  implicit def F: Plus[F]

  def plus[A](a: F[G[A]], b: => F[G[A]]): F[G[A]] =
    F.plus(a, b)
}

private trait CompositionPlusEmpty[F[_], G[_]] extends PlusEmpty[λ[α => F[G[α]]]] with CompositionPlus[F, G] {
  implicit def F: PlusEmpty[F]

  def empty[A]: F[G[A]] = F.empty[G[A]]
}

private trait CompositionApplicativePlus[F[_], G[_]] extends ApplicativePlus[λ[α => F[G[α]]]] with CompositionApplicative[F, G] with CompositionPlusEmpty[F, G] {
  implicit def F: ApplicativePlus[F]

  implicit def G: Applicative[G]
}

private trait CompositionFoldable[F[_], G[_]] extends Foldable[λ[α => F[G[α]]]]  {
  implicit def F: Foldable[F]

  implicit def G: Foldable[G]

  override def foldRight[A, B](fa: F[G[A]], z: => B)(f: (A, => B) => B): B =
    F.foldRight(fa, z)((a, b) => G.foldRight(a, b)(f))

  override def foldMap[A,B](fa: F[G[A]])(f: A => B)(implicit M: Monoid[B]): B =
    F.foldMap(fa)(G.foldMap(_)(f))

  override def foldLeft[A, B](fa: F[G[A]], z: B)(f: (B, A) => B): B =
    F.foldLeft(fa, z)((b, a) => G.foldLeft(a, b)(f))
}

private trait CompositionTraverse[F[_], G[_]] extends Traverse[λ[α => F[G[α]]]] with CompositionFunctor[F, G] with CompositionFoldable[F, G] {
  implicit def F: Traverse[F]

  implicit def G: Traverse[G]

  def traverseImpl[X[_]:Applicative, A, B](a: F[G[A]])(f: A => X[B]): X[F[G[B]]] =
    F.traverse(a)(G.traverse(_)(f))

}

private trait CompositionFoldable1[F[_], G[_]] extends Foldable1[λ[α => F[G[α]]]] with CompositionFoldable[F, G] {
  implicit def F: Foldable1[F]

  implicit def G: Foldable1[G]

  override final def foldMap1[A, B: Semigroup](fa: F[G[A]])(f: A => B) =
    F.foldMap1(fa)(G.foldMap1(_)(f))

  override final def foldMapRight1[A, B](fa: F[G[A]])(z: A => B)(f: (A, => B) => B) =
    F.foldMapRight1(fa)(G.foldMapRight1(_)(z)(f))(G.foldRight(_, _)(f))

  override final def foldMapLeft1[A, B](fa: F[G[A]])(z: A => B)(f: (B, A) => B) =
    F.foldMapLeft1(fa)(G.foldMapLeft1(_)(z)(f))((b, ga) => G.foldLeft(ga, b)(f))
}

private trait CompositionTraverse1[F[_], G[_]] extends Traverse1[λ[α => F[G[α]]]] with CompositionFoldable1[F, G] {
  implicit def F: Traverse1[F]

  implicit def G: Traverse1[G]

  def traverse1Impl[X[_]:Apply, A, B](a: F[G[A]])(f: A => X[B]) =
    F.traverse1(a)(G.traverse1(_)(f))
}

private trait CompositionDistributive[F[_], G[_]] extends Distributive[λ[α => F[G[α]]]] with CompositionFunctor[F, G] {
  implicit def F: Distributive[F]

  implicit def G: Distributive[G]

  def distributeImpl[X[_]:Functor, A, B](a: X[A])(f: A => F[G[B]]): F[G[X[B]]] =
    F(F.distribute(a)(f))(G.cosequence(_))
}

private trait CompositionZip[F[_], G[_]] extends Zip[λ[α => F[G[α]]]] {
  implicit def T: Functor[F]

  implicit def F: Zip[F]

  implicit def G: Zip[G]

  def zip[A, B](a: => F[G[A]], b: => F[G[B]]): F[G[(A, B)]] =
    F.zipWith(a, b)(G.zip(_, _))
}

private trait CompositionUnzip[F[_], G[_]] extends Unzip[λ[α => F[G[α]]]] {
  implicit def T: Functor[F]

  implicit def F: Unzip[F]

  implicit def G: Unzip[G]

  def unzip[A, B](a: F[G[(A, B)]]): (F[G[A]], F[G[B]]) = {
    val f = T.map(a)(G.firsts(_))
    val g = T.map(a)(G.seconds(_))
    (f, g)
  }
}

private trait CompositionBifunctor[F[_, _], G[_, _]] extends Bifunctor[λ[(α, β) => F[G[α, β], G[α, β]]]] {
  implicit def F: Bifunctor[F]

  implicit def G: Bifunctor[G]

  override def bimap[A, B, C, D](fab: F[G[A, B], G[A, B]])(f: A => C, g: B => D): F[G[C, D], G[C, D]] =
    F.bimap(fab)(G.bimap(_)(f, g), G.bimap(_)(f, g))
}

private trait CompositionBifoldable[F[_, _], G[_, _]] extends Bifoldable[λ[(α, β) => F[G[α, β], G[α, β]]]] {
  implicit def F: Bifoldable[F]

  implicit def G: Bifoldable[G]

  override def bifoldMap[A,B,M](fa: F[G[A, B], G[A, B]])(f: A => M)(g: B => M)(implicit M: Monoid[M]): M =
    F.bifoldMap(fa)(G.bifoldMap(_)(f)(g))(G.bifoldMap(_)(f)(g))
  override def bifoldRight[A,B,C](fa: F[G[A, B], G[A, B]], z: => C)(f: (A, => C) => C)(g: (B, => C) => C): C =
    F.bifoldRight(fa, z)((a, b) => G.bifoldRight(a, b)(f)(g))((a, b) => G.bifoldRight(a, b)(f)(g))
  override def bifoldLeft[A,B,C](fa: F[G[A, B], G[A, B]], z: C)(f: (C, A) => C)(g: (C, B) => C): C =
    F.bifoldLeft(fa, z)((b, a) => G.bifoldLeft(a, b)(f)(g))((b, a) => G.bifoldLeft(a, b)(f)(g))
}

private trait CompositionBitraverse[F[_, _], G[_, _]]
  extends Bitraverse[λ[(α, β) => F[G[α, β], G[α, β]]]]
  with CompositionBifunctor[F, G] with CompositionBifoldable[F, G]{

  implicit def F: Bitraverse[F]

  implicit def G: Bitraverse[G]

  def bitraverseImpl[X[_] : Applicative, A, B, C, D](fab: F[G[A, B], G[A, B]])(f: A => X[C], g: B => X[D]): X[F[G[C, D], G[C, D]]] =
    F.bitraverseImpl(fab)(G.bitraverse(_)(f)(g), G.bitraverse(_)(f)(g))

}

private trait CompositionFunctorBifunctor[F[_], G[_, _]] extends Bifunctor[λ[(α, β) => F[G[α, β]]]] {
  def F: Functor[F]

  def G: Bifunctor[G]

  override def bimap[A, B, C, D](fab: F[G[A, B]])(f: A => C, g: B => D) =
    F.map(fab)(G.bimap(_)(f, g))
}

private trait CompositionBifunctorFunctors[F[_,_], G[_], H[_]] extends Bifunctor[λ[(α, β) => F[G[α],H[β]]]] {
  def F: Bifunctor[F]

  def G: Functor[G]

  def H: Functor[H]

  override def bimap[A, B, C, D](fgahb: F[G[A], H[B]])(f: A => C, g: B => D): F[G[C], H[D]] =
    F.bimap(fgahb)(G.lift(f), H.lift(g))
}

private trait CompositionFoldableBifoldable[F[_], G[_, _]] extends Bifoldable[λ[(α, β) => F[G[α, β]]]] {
  def F: Foldable[F]

  def G: Bifoldable[G]

  override def bifoldMap[A, B, M: Monoid](fa: F[G[A, B]])(f: A => M)(g: B => M) =
    F.foldMap(fa)(G.bifoldMap(_)(f)(g))
  override def bifoldRight[A, B, C](fa: F[G[A, B]], z: => C)(f: (A, => C) => C)(g: (B, => C) => C) =
    F.foldRight(fa, z)(G.bifoldRight(_, _)(f)(g))
  override def bifoldLeft[A, B, C](fa: F[G[A, B]], z: C)(f: (C, A) => C)(g: (C, B) => C) =
    F.foldLeft(fa, z)((c, gab) => G.bifoldLeft(gab, c)(f)(g))
}

private trait CompositionBifoldableFoldables[F[_,_], G[_], H[_]] extends Bifoldable[λ[(α, β) => F[G[α],H[β]]]] {
  def F: Bifoldable[F]

  def G: Foldable[G]

  def H: Foldable[H]

  override def bifoldMap[A, B, M: Monoid](fgahb: F[G[A], H[B]])(f: A => M)(g: B => M) =
    F.bifoldMap(fgahb)( ga => G.foldMap(ga)(f) )( hb => H.foldMap(hb)(g) )
  override def bifoldRight[A, B, C](fgahb: F[G[A],H[B]], z: => C)(f: (A, => C) => C)(g: (B, => C) => C) =
    F.bifoldRight(fgahb, z)( (ga, c) => G.foldRight(ga,c)(f) )( (hb,c) => H.foldRight(hb,c)(g) )
  override def bifoldLeft[A, B, C](fgahb: F[G[A],H[B]], z: C)(f: (C, A) => C)(g: (C, B) => C) =
    F.bifoldLeft(fgahb, z)( (c,ga) => G.foldLeft(ga,c)(f) )( (c,hb) => H.foldLeft(hb,c)(g) )
}

private trait CompositionTraverseBitraverse[F[_], G[_, _]]
  extends Bitraverse[λ[(α, β) => F[G[α, β]]]]
  with CompositionFunctorBifunctor[F, G]
  with CompositionFoldableBifoldable[F, G] {
  def F: Traverse[F]

  def G: Bitraverse[G]

  override def bitraverseImpl[H[_]: Applicative, A, B, C, D](fab: F[G[A, B]])(f: A => H[C], g: B => H[D]) =
    F.traverseImpl(fab)(G.bitraverseF(f, g))
}

private trait CompositionBitraverseTraverses[F[_,_], G[_], H[_]]
  extends Bitraverse[λ[(α, β) => F[G[α], H[β]]]]
  with CompositionBifunctorFunctors[F, G, H]
  with CompositionBifoldableFoldables[F, G, H] {
    def F: Bitraverse[F]

    def G: Traverse[G]

    def H: Traverse[H]

    override def bitraverseImpl[K[_] : Applicative, A, B, C, D](fgahb: F[G[A], H[B]])(f: A => K[C], g: B => K[D]): K[F[G[C], H[D]]] =
      F.bitraverseImpl(fgahb)( ga => G.traverseImpl(ga)(f) , hb => H.traverseImpl(hb)(g) )
}
