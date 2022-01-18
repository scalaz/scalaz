package scalaz

private trait FreeFoldable[F[_]] extends Foldable[Free[F, *]] {
  def F: Foldable[F]

  override final def foldMap[A, B: Monoid](fa: Free[F, A])(f: A => B): B =
    fa.foldStep(
      f,
      fa => F.foldMap(fa)(f),
      { case (fx, g) => F.foldMap(fx)(x => foldMap(g(x))(f)) }
    )

  override final def foldLeft[A, B](fa: Free[F, A], z: B)(f: (B, A) => B): B =
    fa.foldStep(
      a => f(z, a),
      fa => F.foldLeft(fa, z)(f),
      { case (fx, g) => F.foldLeft(fx, z)((b, x) => foldLeft(g(x), b)(f)) }
    )

  override final def foldRight[A, B](fa: Free[F, A], z: => B)(f: (A, => B) => B): B =
    fa.foldStep(
      a => f(a, z),
      fa => F.foldRight(fa, z)(f),
      { case (fx, g) => F.foldRight(fx, z)((x, b) => foldRight(g(x), b)(f)) }
    )
}

private trait FreeFoldable1[F[_]] extends Foldable1[Free[F, *]] {
  def F: Foldable1[F]

  override final def foldMap1[A, B: Semigroup](fa: Free[F, A])(f: A => B): B =
    fa.foldStep(
      f,
      fa => F.foldMap1(fa)(f),
      { case (fx, g) => F.foldMap1(fx)(x => foldMap1(g(x))(f)) }
    )

  override final def foldMapRight1[A, B](fa: Free[F, A])(z: A => B)(f: (A, => B) => B): B =
    fa.foldStep(
      z,
      fa => F.foldMapRight1(fa)(z)(f),
      { case (fx, g) => F.foldMapRight1(fx)(x => foldMapRight1(g(x))(z)(f))((x, b) => foldRight(g(x), b)(f)) }
    )

  override final def foldMapLeft1[A, B](fa: Free[F, A])(z: A => B)(f: (B, A) => B): B =
    fa.foldStep(
      z,
      fa => F.foldMapLeft1(fa)(z)(f),
      { case (fx, g) => F.foldMapLeft1(fx)(x => foldMapLeft1(g(x))(z)(f))((b, x) => foldLeft(g(x), b)(f)) }
    )
}

