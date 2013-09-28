package scalaz

import std.option.cata

private trait ProductFunctor[F[_], G[_]] extends Functor[({type λ[α] = (F[α], G[α])})#λ] {
  implicit def F: Functor[F]

  implicit def G: Functor[G]

  override def map[A, B](fa: (F[A], G[A]))(f: A => B): (F[B], G[B]) = (F.map(fa._1)(f), G.map(fa._2)(f))
}

private trait ProductApply[F[_], G[_]] extends Apply[({type λ[α] = (F[α], G[α])})#λ] with ProductFunctor[F, G] {
  implicit def F: Apply[F]

  implicit def G: Apply[G]

  def ap[A, B](fa: => (F[A], G[A]))(f: => (F[A => B], G[A => B])): (F[B], G[B]) =
    (F.ap(fa._1)(f._1), G.ap(fa._2)(f._2))
}

private trait ProductApplicative[F[_], G[_]] extends Applicative[({type λ[α] = (F[α], G[α])})#λ] with ProductApply[F, G] {
  implicit def F: Applicative[F]

  implicit def G: Applicative[G]

  def point[A](a: => A): (F[A], G[A]) = (F.point(a), G.point(a))
}

private trait ProductPlus[F[_], G[_]] extends Plus[({type λ[α] = (F[α], G[α])})#λ] {
  implicit def F: Plus[F]

  implicit def G: Plus[G]

  def plus[A](a: (F[A], G[A]), b: => (F[A], G[A])): (F[A], G[A]) =
    (F.plus(a._1, b._1), G.plus(a._2, b._2))
}

private trait ProductPlusEmpty[F[_], G[_]] extends PlusEmpty[({type λ[α] = (F[α], G[α])})#λ] with ProductPlus[F, G] {
  implicit def F: PlusEmpty[F]

  implicit def G: PlusEmpty[G]

  def empty[A]: (F[A], G[A]) = (F.empty[A], G.empty[A])
}

private trait ProductApplicativePlus[F[_], G[_]] extends ApplicativePlus[({type λ[α] = (F[α], G[α])})#λ] with ProductApplicative[F, G] with ProductPlusEmpty[F, G] {
  implicit def F: ApplicativePlus[F]

  implicit def G: ApplicativePlus[G]
}

private trait ProductFoldable[F[_], G[_]] extends Foldable[({type λ[α] = (F[α], G[α])})#λ] {
  implicit def F: Foldable[F]

  implicit def G: Foldable[G]

  override def foldRight[A, B](fa: (F[A], G[A]), z: => B)(f: (A, => B) => B): B =
    F.foldRight(fa._1, G.foldRight(fa._2, z)(f))(f)

  override def foldMap[A,B](fa: (F[A], G[A]))(f: A => B)(implicit M: Monoid[B]): B =
    M.append(F.foldMap(fa._1)(f), G.foldMap(fa._2)(f))

  override def foldLeft[A, B](fa: (F[A], G[A]), z: B)(f: (B, A) => B): B =
    G.foldLeft(fa._2, F.foldLeft(fa._1, z)(f))(f)
}

private trait ProductFoldable1L[F[_], G[_]] extends Foldable1[({type λ[α] = (F[α], G[α])})#λ] with ProductFoldable[F, G] {
  implicit def F: Foldable1[F]

  override def foldMapRight1[A, B](fa: (F[A], G[A]))(z: A => B)(f: (A, => B) => B): B =
    cata(G.foldMapRight1Opt(fa._2)(z)(f))(F.foldRight(fa._1, _)(f), F.foldMapRight1(fa._1)(z)(f))

  override def foldMap1[A,B](fa: (F[A], G[A]))(f: A => B)(implicit S: Semigroup[B]): B = {
    val resume = F.foldMap1(fa._1)(f)
    cata(G.foldMap1Opt(fa._2)(f))(S.append(resume, _), resume)
  }

  override def foldMapLeft1[A, B](fa: (F[A], G[A]))(z: A => B)(f: (B, A) => B): B =
    G.foldLeft(fa._2, F.foldMapLeft1(fa._1)(z)(f))(f)
}

private trait ProductFoldable1R[F[_], G[_]] extends Foldable1[({type λ[α] = (F[α], G[α])})#λ] with ProductFoldable[F, G] {
  implicit def G: Foldable1[G]

  override def foldMapRight1[A, B](fa: (F[A], G[A]))(z: A => B)(f: (A, => B) => B): B =
    F.foldRight(fa._1, G.foldMapRight1(fa._2)(z)(f))(f)

  override def foldMap1[A,B](fa: (F[A], G[A]))(f: A => B)(implicit S: Semigroup[B]): B = {
    def resume = G.foldMap1(fa._2)(f)
    cata(F.foldMap1Opt(fa._1)(f))(S.append(_, resume), resume)
  }

  override def foldMapLeft1[A, B](fa: (F[A], G[A]))(z: A => B)(f: (B, A) => B): B =
    cata(F.foldMapLeft1Opt(fa._1)(z)(f))(G.foldLeft(fa._2, _)(f), G.foldMapLeft1(fa._2)(z)(f))
}

private trait ProductFoldable1[F[_], G[_]] extends Foldable1[({type λ[α] = (F[α], G[α])})#λ] with ProductFoldable[F, G] {
  implicit def F: Foldable1[F]

  implicit def G: Foldable1[G]

  override def foldMapRight1[A, B](fa: (F[A], G[A]))(z: A => B)(f: (A, => B) => B): B =
    F.foldRight(fa._1, G.foldMapRight1(fa._2)(z)(f))(f)

  override def foldMap1[A,B](fa: (F[A], G[A]))(f: A => B)(implicit S: Semigroup[B]): B =
    S.append(F.foldMap1(fa._1)(f), G.foldMap1(fa._2)(f))

  override def foldMapLeft1[A, B](fa: (F[A], G[A]))(z: A => B)(f: (B, A) => B): B =
    G.foldLeft(fa._2, F.foldMapLeft1(fa._1)(z)(f))(f)
}

private trait ProductTraverse[F[_], G[_]] extends Traverse[({type λ[α] = (F[α], G[α])})#λ] with ProductFunctor[F, G] with ProductFoldable[F, G] {
  implicit def F: Traverse[F]

  implicit def G: Traverse[G]

  def traverseImpl[X[_]:Applicative, A, B](a: (F[A], G[A]))(f: A => X[B]): X[(F[B], G[B])] =
    Applicative[X].apply2(F.traverse(a._1)(f), G.traverse(a._2)(f))((a, b) => (a, b))
}

private trait ProductTraverse1L[F[_], G[_]] extends Traverse1[({type λ[α] = (F[α], G[α])})#λ] with ProductFoldable1L[F, G] with ProductTraverse[F, G] {
  implicit def F: Traverse1[F]

  def traverse1Impl[X[_], A, B](a: (F[A], G[A]))(f: A => X[B])(implicit X0: Apply[X]): X[(F[B], G[B])] = {
    def resume = F.traverse1(a._1)(f)
    X0.applyApplicative.traverse(a._2)(a => -\/(f(a)))(G)
      .fold(X0.apply2(resume, _)(Tuple2.apply),
            pr => X0.map(resume)((_, pr)))
  }

  override def traverseImpl[X[_]:Applicative, A, B](a: (F[A], G[A]))(f: A => X[B]): X[(F[B], G[B])] =
    super[ProductTraverse].traverseImpl(a)(f)
}

private trait ProductTraverse1R[F[_], G[_]] extends Traverse1[({type λ[α] = (F[α], G[α])})#λ] with ProductFoldable1R[F, G] with ProductTraverse[F, G] {
  implicit def G: Traverse1[G]

  def traverse1Impl[X[_], A, B](a: (F[A], G[A]))(f: A => X[B])(implicit X0: Apply[X]): X[(F[B], G[B])] = {
    def resume = G.traverse1(a._2)(f)
    X0.applyApplicative.traverse(a._1)(a => -\/(f(a)))(F)
      .fold(X0.apply2(_, resume)(Tuple2.apply),
            pr => X0.map(resume)((pr, _)))
  }

  override def traverseImpl[X[_]:Applicative, A, B](a: (F[A], G[A]))(f: A => X[B]): X[(F[B], G[B])] =
    super[ProductTraverse].traverseImpl(a)(f)
}

private trait ProductTraverse1[F[_], G[_]] extends Traverse1[({type λ[α] = (F[α], G[α])})#λ] with ProductFoldable1[F, G] with ProductTraverse[F, G] {
  implicit def F: Traverse1[F]

  implicit def G: Traverse1[G]

  def traverse1Impl[X[_]:Apply, A, B](a: (F[A], G[A]))(f: A => X[B]): X[(F[B], G[B])] =
    Apply[X].apply2(F.traverse1(a._1)(f), G.traverse1(a._2)(f))((a, b) => (a, b))

  override def traverseImpl[X[_]:Applicative, A, B](a: (F[A], G[A]))(f: A => X[B]): X[(F[B], G[B])] =
    super[ProductTraverse].traverseImpl(a)(f)
}

private trait ProductDistributive[F[_], G[_]] extends Distributive[({type λ[α] = (F[α], G[α])})#λ] with ProductFunctor[F, G] {
  implicit def F: Distributive[F]

  implicit def G: Distributive[G]

  def distributeImpl[X[_]:Functor, A, B](a: X[A])(f: A => (F[B], G[B])): (F[X[B]], G[X[B]]) =
    (F.distribute(a)(x => f(x)._1), G.distribute(a)(x => f(x)._2))
}

private trait ProductAlign[F[_], G[_]] extends Align[({type λ[α] = (F[α], G[α])})#λ] with ProductFunctor[F, G] {
  implicit def F: Align[F]

  implicit def G: Align[G]

  def alignWith[A, B, C](f: (A \&/ B) => C): ((F[A], G[A]), (F[B], G[B])) => (F[C], G[C]) = {
    case ((fa, ga), (fb, gb)) => (F.alignWith(f)(fa, fb), G.alignWith(f)(ga, gb))
  }
}

private trait ProductZip[F[_], G[_]] extends Zip[({type λ[α] = (F[α], G[α])})#λ] {
  implicit def F: Zip[F]

  implicit def G: Zip[G]

  def zip[A, B](a: => (F[A], G[A]), b: => (F[B], G[B])): (F[(A, B)], G[(A, B)]) =
    (F.zip(a._1, b._1), G.zip(a._2, b._2))
}

private trait ProductUnzip[F[_], G[_]] extends Unzip[({type λ[α] = (F[α], G[α])})#λ] {
  implicit def F: Unzip[F]

  implicit def G: Unzip[G]

  def unzip[A, B](x: (F[(A, B)], G[(A, B)])): ((F[A], G[A]), (F[B], G[B])) = {
    val (fa, fb) = F.unzip(x._1)
    val (ga, gb) = G.unzip(x._2)
    ((fa, ga), (fb, gb))
  }
}

private trait ProductBifunctor[F[_, _], G[_, _]] extends Bifunctor[({type λ[α, β]=(F[α, β], G[α, β])})#λ] {
  implicit def F: Bifunctor[F]

  implicit def G: Bifunctor[G]

  override def bimap[A, B, C, D](fab: (F[A, B], G[A, B]))(f: A => C, g: B => D): (F[C, D], G[C, D]) =
    (F.bimap(fab._1)(f, g), G.bimap(fab._2)(f, g))
}

private trait ProductBifoldable[F[_, _], G[_, _]] extends Bifoldable[({type λ[α, β]=(F[α, β], G[α, β])})#λ] {
  implicit def F: Bifoldable[F]

  implicit def G: Bifoldable[G]

  override def bifoldMap[A,B,M](fa: (F[A, B], G[A, B]))(f: A => M)(g: B => M)(implicit M: Monoid[M]): M =
    M.append(F.bifoldMap(fa._1)(f)(g), G.bifoldMap(fa._2)(f)(g))
  override def bifoldRight[A,B,C](fa: (F[A, B], G[A, B]), z: => C)(f: (A, => C) => C)(g: (B, => C) => C): C =
    F.bifoldRight(fa._1, G.bifoldRight(fa._2, z)(f)(g))(f)(g)
  override def bifoldLeft[A,B,C](fa: (F[A, B], G[A, B]), z: C)(f: (C, A) => C)(g: (C, B) => C): C =
    G.bifoldLeft( fa._2, F.bifoldLeft( fa._1, z)(f)(g))(f)(g)
}

private trait ProductBitraverse[F[_, _], G[_, _]]
  extends Bitraverse[({type λ[α, β]=(F[α, β], G[α, β])})#λ] with ProductBifunctor[F, G] with ProductBifoldable[F, G] {

  implicit def F: Bitraverse[F]

  implicit def G: Bitraverse[G]

  def bitraverseImpl[X[_] : Applicative, A, B, C, D](x: (F[A, B], G[A, B]))(f: A => X[C], g: B => X[D]): X[(F[C, D], G[C, D])] =
    Applicative[X].apply2(F.bitraverse(x._1)(f)(g), G.bitraverse(x._2)(f)(g))((a, b) => (a, b))
}
