package scalaz

private trait LeftFunctor[F[_,_], X] extends Functor[F[*, X]] {
  implicit def F: Bifunctor[F]

  override def map[A, C](fax: F[A, X])(f: A => C): F[C, X] =
    F.bimap(fax)(f, identity)
}

private trait RightFunctor[F[_,_], X] extends Functor[F[X, *]] {
  implicit def F: Bifunctor[F]

  override def map[A, C](fax: F[X, A])(f: A => C): F[X, C] =
    F.bimap(fax)(identity, f)
}

private trait UFunctor[F[_,_]] extends Functor[λ[α => F[α, α]]] {
  implicit def F: Bifunctor[F]

  override def map[A, C](fax: F[A, A])(f: A => C): F[C, C] =
    F.bimap(fax)(f, f)
}

private trait LeftFoldable[F[_,_], X] extends Foldable[F[*, X]] {
  implicit def F: Bifoldable[F]

  override def foldMap[A,B](fa: F[A, X])(f: A => B)(implicit B: Monoid[B]): B =
    F.bifoldMap(fa)(f)(Function const B.zero)

  override def foldRight[A, B](fa: F[A, X], z: => B)(f: (A, => B) => B): B =
    F.bifoldRight(fa, z)(f)((_, b) => b)

  override def foldLeft[A, B](fa: F[A, X], z: B)(f: (B, A) => B): B =
    F.bifoldLeft(fa, z)(f)((b, _) => b)
}

private trait RightFoldable[F[_,_], X] extends Foldable[F[X, *]] {
  implicit def F: Bifoldable[F]

  override def foldMap[A,B](fa: F[X, A])(f: A => B)(implicit B: Monoid[B]): B =
    F.bifoldMap(fa)(Function const B.zero)(f)

  override def foldRight[A, B](fa: F[X, A], z: => B)(f: (A, => B) => B): B =
    F.bifoldRight(fa, z)((_, b) => b)(f)

  override def foldLeft[A, B](fa: F[X, A], z: B)(f: (B, A) => B): B =
    F.bifoldLeft(fa, z)((b, _) => b)(f)
}

private trait UFoldable[F[_,_]] extends Foldable[λ[α => F[α, α]]] {
  implicit def F: Bifoldable[F]

  override def foldMap[A,B](fa: F[A, A])(f: A => B)(implicit B: Monoid[B]): B =
    F.bifoldMap(fa)(f)(f)

  override def foldRight[A, B](fa: F[A, A], z: => B)(f: (A, => B) => B): B =
    F.bifoldRight(fa, z)(f)(f)

  override def foldLeft[A, B](fa: F[A, A], z: B)(f: (B, A) => B): B =
    F.bifoldLeft(fa, z)(f)(f)
}

private trait LeftTraverse[F[_,_], X] extends Traverse[F[*, X]]
    with LeftFunctor[F, X] with LeftFoldable[F, X] {
  implicit def F: Bitraverse[F]

  def traverseImpl[G[_]:Applicative,A,B](fa: F[A, X])(f: A => G[B]): G[F[B, X]] =
    F.bitraverseImpl(fa)(f, x => Applicative[G] point x)
}

private trait RightTraverse[F[_,_], X] extends Traverse[F[X, *]]
    with RightFunctor[F, X] with RightFoldable[F, X] {
  implicit def F: Bitraverse[F]

  def traverseImpl[G[_]:Applicative,A,B](fa: F[X, A])(f: A => G[B]): G[F[X, B]] =
    F.bitraverseImpl(fa)(x => Applicative[G] point x, f)
}

private trait UTraverse[F[_,_]] extends Traverse[λ[α => F[α, α]]]
    with UFunctor[F] with UFoldable[F] {
  implicit def F: Bitraverse[F]

  def traverseImpl[G[_]:Applicative,A,B](fa: F[A, A])(f: A => G[B]): G[F[B, B]] =
    F.bitraverseImpl(fa)(f, f)
}
