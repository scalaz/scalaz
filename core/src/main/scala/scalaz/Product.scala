package scalaz


private[scalaz] trait ProductFunctor[F[_], G[_]] extends Functor[({type λ[α] = (F[α], G[α])})#λ] {
  implicit def F: Functor[F]

  implicit def G: Functor[G]

  override def map[A, B](fa: (F[A], G[A]))(f: A => B): (F[B], G[B]) = (F.map(fa._1)(f), G.map(fa._2)(f))
}

private[scalaz] trait ProductApply[F[_], G[_]] extends Apply[({type λ[α] = (F[α], G[α])})#λ] with ProductFunctor[F, G] {
  implicit def F: Apply[F]

  implicit def G: Apply[G]

  def ap[A, B](fa: => (F[A], G[A]))(f: => (F[A => B], G[A => B])): (F[B], G[B]) =
    (F.ap(fa._1)(f._1), G.ap(fa._2)(f._2))
}

private[scalaz] trait ProductApplicative[F[_], G[_]] extends Applicative[({type λ[α] = (F[α], G[α])})#λ] with ProductApply[F, G] {
  implicit def F: Applicative[F]

  implicit def G: Applicative[G]

  def point[A](a: => A): (F[A], G[A]) = (F.point(a), G.point(a))
}

private[scalaz] trait ProductApplicativePlus[F[_], G[_]] extends ApplicativePlus[({type λ[α] = (F[α], G[α])})#λ] with ProductApplicative[F, G] {
  implicit def F: ApplicativePlus[F]

  implicit def G: ApplicativePlus[G]

  def empty[A]: (F[A], G[A]) = (F.empty[A], G.empty[A])
  def plus[A](a: (F[A], G[A]), b: => (F[A], G[A])): (F[A], G[A]) =
    (F.plus(a._1, b._1), G.plus(a._2, b._2))

}

private[scalaz] trait ProductFoldable[F[_], G[_]] extends Foldable[({type λ[α] = (F[α], G[α])})#λ] {
  implicit def F: Foldable[F]

  implicit def G: Foldable[G]

  override def foldRight[A, B](fa: (F[A], G[A]), z: => B)(f: (A, => B) => B): B =
    F.foldRight(fa._1, G.foldRight(fa._2, z)(f))(f)

  override def foldMap[A,B](fa: (F[A], G[A]))(f: A => B)(implicit M: Monoid[B]): B =
    M.append(F.foldMap(fa._1)(f), G.foldMap(fa._2)(f))

  override def foldLeft[A, B](fa: (F[A], G[A]), z: B)(f: (B, A) => B): B =
    G.foldLeft(fa._2, F.foldLeft(fa._1, z)(f))(f)
}

private[scalaz] trait ProductFoldable1[F[_], G[_]] extends Foldable1[({type λ[α] = (F[α], G[α])})#λ] {
  implicit def F: Foldable1[F]

  implicit def G: Foldable1[G]

  override def foldRight1[A](fa: (F[A], G[A]))(f: (A, => A) => A): A =
    F.foldRight(fa._1, G.foldRight1(fa._2)(f))(f)

  override def foldMap1[A,B](fa: (F[A], G[A]))(f: A => B)(implicit S: Semigroup[B]): B =
    S.append(F.foldMap1(fa._1)(f), G.foldMap1(fa._2)(f))

  override def foldLeft1[A](fa: (F[A], G[A]))(f: (A, A) => A): A =
    G.foldLeft(fa._2, F.foldLeft1(fa._1)(f))(f)
}

private[scalaz] trait ProductTraverse[F[_], G[_]] extends Traverse[({type λ[α] = (F[α], G[α])})#λ] with ProductFunctor[F, G] with ProductFoldable[F, G] {
  implicit def F: Traverse[F]

  implicit def G: Traverse[G]

  def traverseImpl[X[_]:Applicative, A, B](a: (F[A], G[A]))(f: A => X[B]): X[(F[B], G[B])] =
    Applicative[X].apply2(F.traverse(a._1)(f), G.traverse(a._2)(f))((a, b) => (a, b))
}

private[scalaz] trait ProductTraverse1[F[_], G[_]] extends Traverse1[({type λ[α] = (F[α], G[α])})#λ] with ProductFunctor[F, G] with ProductFoldable1[F, G] {
  implicit def F: Traverse1[F]

  implicit def G: Traverse1[G]

  def traverse1Impl[X[_]:Apply, A, B](a: (F[A], G[A]))(f: A => X[B]): X[(F[B], G[B])] =
    Apply[X].apply2(F.traverse1(a._1)(f), G.traverse1(a._2)(f))((a, b) => (a, b))
}

private[scalaz] trait ProductDistributive[F[_], G[_]] extends Distributive[({type λ[α] = (F[α], G[α])})#λ] with ProductFunctor[F, G] {
  implicit def F: Distributive[F]

  implicit def G: Distributive[G]

  def distributeImpl[X[_]:Functor, A, B](a: X[A])(f: A => (F[B], G[B])): (F[X[B]], G[X[B]]) =
    (F.distribute(a)(x => f(x)._1), G.distribute(a)(x => f(x)._2))
}

private[scalaz] trait ProductZip[F[_], G[_]] extends Zip[({type λ[α] = (F[α], G[α])})#λ] {
  implicit def F: Zip[F]

  implicit def G: Zip[G]

  def zip[A, B](a: => (F[A], G[A]), b: => (F[B], G[B])): (F[(A, B)], G[(A, B)]) =
    (F.zip(a._1, b._1), G.zip(a._2, b._2))
}

private[scalaz] trait ProductUnzip[F[_], G[_]] extends Unzip[({type λ[α] = (F[α], G[α])})#λ] {
  implicit def F: Unzip[F]

  implicit def G: Unzip[G]

  def unzip[A, B](x: (F[(A, B)], G[(A, B)])): ((F[A], G[A]), (F[B], G[B])) = {
    val (fa, fb) = F.unzip(x._1)
    val (ga, gb) = G.unzip(x._2)
    ((fa, ga), (fb, gb))
  }
}

private[scalaz] trait ProductBifunctor[F[_, _], G[_, _]] extends Bifunctor[({type λ[α, β]=(F[α, β], G[α, β])})#λ] {
  implicit def F: Bifunctor[F]

  implicit def G: Bifunctor[G]

  override def bimap[A, B, C, D](fab: (F[A, B], G[A, B]))(f: A => C, g: B => D): (F[C, D], G[C, D]) =
    (F.bimap(fab._1)(f, g), G.bimap(fab._2)(f, g))
}

private[scalaz] trait ProductBifoldable[F[_, _], G[_, _]] extends Bifoldable[({type λ[α, β]=(F[α, β], G[α, β])})#λ] {
  implicit def F: Bifoldable[F]

  implicit def G: Bifoldable[G]

  override def bifoldMap[A,B,M](fa: (F[A, B], G[A, B]))(f: A => M)(g: B => M)(implicit M: Monoid[M]): M =
    M.append(F.bifoldMap(fa._1)(f)(g), G.bifoldMap(fa._2)(f)(g))
  override def bifoldRight[A,B,C](fa: (F[A, B], G[A, B]), z: => C)(f: (A, => C) => C)(g: (B, => C) => C): C =
    F.bifoldRight(fa._1, G.bifoldRight(fa._2, z)((a, b) => f(a, b))((b, c) => g(b, c)))((a, b) => f(a, b))((b, c) => g(b, c))
  override def bifoldLeft[A,B,C](fa: (F[A, B], G[A, B]), z: C)(f: (C, A) => C)(g: (C, B) => C): C =
    F.bifoldLeft(fa._1, G.bifoldLeft(fa._2, z)((b, a) => f(b, a))((c, b) => g(c, b)))((b, a) => f(b, a))((c, b) => g(c, b))

}

private[scalaz] trait ProductBitraverse[F[_, _], G[_, _]]
  extends Bitraverse[({type λ[α, β]=(F[α, β], G[α, β])})#λ] with ProductBifunctor[F, G] with ProductBifoldable[F, G] {

  implicit def F: Bitraverse[F]

  implicit def G: Bitraverse[G]

  def bitraverseImpl[X[_] : Applicative, A, B, C, D](x: (F[A, B], G[A, B]))(f: A => X[C], g: B => X[D]): X[(F[C, D], G[C, D])] =
    Applicative[X].apply2(F.bitraverse(x._1)(f)(g), G.bitraverse(x._2)(f)(g))((a, b) => (a, b))
}
