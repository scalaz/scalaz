package scalaz

private trait FreeFoldable[F[_]] extends Foldable[Free[F, *]] {
  def F: Foldable[F]

  override final def foldMap[A, B: Monoid](fa: Free[F, A])(f: A => B): B =
    fa.foldStep(
      f,
      fa => F.foldMap(fa)(f),
      new ~>[({type l[a] = (F[a], a => Free[F, A])})#l, ({type l[a] = B})#l] {
        override def apply[X](a: (F[X], X => Free[F, A])) =
          F.foldMap(a._1)(x => foldMap(a._2 apply x)(f))
      }
    )

  override final def foldLeft[A, B](fa: Free[F, A], z: B)(f: (B, A) => B): B =
    fa.foldStep(
      a => f(z, a),
      fa => F.foldLeft(fa, z)(f),
      new ~>[({type l[a] = (F[a], a => Free[F, A])})#l, ({type l[a] = B})#l] {
        override def apply[X](a: (F[X], X => Free[F, A])) =
          F.foldLeft(a._1, z)((b, x) => foldLeft(a._2 apply x, b)(f))
      }
    )

  override final def foldRight[A, B](fa: Free[F, A], z: => B)(f: (A, => B) => B): B =
    fa.foldStep(
      a => f(a, z),
      fa => F.foldRight(fa, z)(f),
      new ~>[({type l[a] = (F[a], a => Free[F, A])})#l, ({type l[a] = B})#l] {
        override def apply[X](a: (F[X], X => Free[F, A])) =
          F.foldRight(a._1, z)((x, b) => foldRight(a._2 apply x, b)(f))
      }
    )
}

private trait FreeFoldable1[F[_]] extends Foldable1[Free[F, *]] {
  def F: Foldable1[F]

  override final def foldMap1[A, B: Semigroup](fa: Free[F, A])(f: A => B): B =
    fa.foldStep(
      f,
      fa => F.foldMap1(fa)(f),
      new ~>[({type l[a] = (F[a], a => Free[F, A])})#l, ({type l[a] = B})#l] {
        override def apply[X](a: (F[X], X => Free[F, A])) =
          F.foldMap1(a._1)(x => foldMap1(a._2 apply x)(f))
      }
    )

  override final def foldMapRight1[A, B](fa: Free[F, A])(z: A => B)(f: (A, => B) => B): B =
    fa.foldStep(
      z,
      fa => F.foldMapRight1(fa)(z)(f),
      new ~>[({type l[a] = (F[a], a => Free[F, A])})#l, ({type l[a] = B})#l] {
        override def apply[X](a: (F[X], X => Free[F, A])) =
          F.foldMapRight1(a._1)(x => foldMapRight1(a._2 apply x)(z)(f))((x, b) => foldRight(a._2 apply x, b)(f))
      }
    )

  override final def foldMapLeft1[A, B](fa: Free[F, A])(z: A => B)(f: (B, A) => B): B =
    fa.foldStep(
      z,
      fa => F.foldMapLeft1(fa)(z)(f),
      new ~>[({type l[a] = (F[a], a => Free[F, A])})#l, ({type l[a] = B})#l] {
        override def apply[X](a: (F[X], X => Free[F, A])) =
          F.foldMapLeft1(a._1)(x => foldMapLeft1(a._2 apply x)(z)(f))((b, x) => foldLeft(a._2 apply x, b)(f))
      }
    )
}

