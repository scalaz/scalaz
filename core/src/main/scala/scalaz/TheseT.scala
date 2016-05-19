package scalaz

final case class TheseT[F[_], A, B](run: F[A \&/ B]) {

  def map[C](f: B => C)(implicit F: Functor[F]): TheseT[F, A, C] =
    TheseT(F.map(run)(_ map f))

  def flatMap[AA >: A, C](f: B => TheseT[F, AA, C])(implicit M: Monad[F], S: Semigroup[AA]): TheseT[F, AA, C]
  = TheseT(M.bind(run) {
    case \&/.This(a)     => M.point(\&/.This(a))
    case \&/.That(b)     => f(b).run
    case \&/.Both(aa, b) => M.map(f(b).run) {
      case \&/.This(a)    => \&/.This(S.append(aa, a))
      case \&/.That(c)    => \&/.Both(aa, c)
      case \&/.Both(a, c) => \&/.Both(S.append(aa, a), c)
    }
  })
  def flatMapF[AA >: A, C](f: B => F[AA \&/ C])(implicit M: Monad[F], S: Semigroup[AA]): TheseT[F, AA, C]
  = flatMap[AA, C](f andThen (x => TheseT(x)))


  def swap(implicit F: Functor[F]): TheseT[F, B, A]
  = TheseT(F.map(run)(_.swap))
  def unary_~(implicit F: Functor[F]): TheseT[F, B, A]
  = swap


  def isThis(implicit F: Functor[F]): F[Boolean] = F.map(run)(_.isThis)
  def isThat(implicit F: Functor[F]): F[Boolean] = F.map(run)(_.isThat)
  def isBoth(implicit F: Functor[F]): F[Boolean] = F.map(run)(_.isBoth)

  def onlyThis(implicit F: Functor[F]): OptionT[F, A] = OptionT(F.map(run)(_.onlyThis))
  def onlyThat(implicit F: Functor[F]): OptionT[F, B] = OptionT(F.map(run)(_.onlyThat))
  def onlyThisOrThat(implicit F: Functor[F]): OptionT[F, A \/ B] = OptionT(F.map(run)(_.onlyThisOrThat))
  def onlyBoth(implicit F: Functor[F]): OptionT[F, (A, B)] = OptionT(F.map(run)(_.onlyBoth))
  def pad(implicit F: Functor[F]): F[(Option[A], Option[B])] = F.map(run)(_.pad)
  def fold[X](s: A => X, t: B => X, q: (A, B) => X)(implicit F: Functor[F]): F[X] = F.map(run)(_.fold(s, t, q))

  def exists(p: B => Boolean)(implicit F: Functor[F]): F[Boolean] = F.map(run)(_ exists p)
  def forall(p: B => Boolean)(implicit F: Functor[F]): F[Boolean] = F.map(run)(_ forall p)
  def toList(implicit F: Functor[F]): ListT[F, B] = ListT(F.map(run)(_.toList))

  def getOrElse(default: => B)(implicit F: Functor[F]): F[B]
  = this.b.getOrElse(default)
  def |(default: => B)(implicit F: Functor[F]): F[B]
  = getOrElse(default)

  def valueOr[BB >: B](x: A => BB)(implicit M: Semigroup[BB], F: Functor[F]): F[BB]
  = F.map(run)(_ valueOr x)

  def swapped[AA, BB](k: (B \&/ A) => (BB \&/ AA))(implicit F: Functor[F]): TheseT[F, AA,  BB] =
    TheseT(F.map(run)(_.swapped(k)))

  def ~[AA, BB](k: (B \&/ A) => (BB \&/ AA))(implicit F: Functor[F]): TheseT[F, AA,  BB] =
    swapped(k)

  def a(implicit F: Functor[F]): OptionT[F, A] = OptionT(F.map(run)(_.a))
  def b(implicit F: Functor[F]): OptionT[F, B] = OptionT(F.map(run)(_.b))

  def append[AA >: A, BB >: B](that: => TheseT[F, AA,  BB])(implicit F: Apply[F], SA: Semigroup[AA], SB: Semigroup[BB]): TheseT[F, AA,  BB]
  = TheseT(F.apply2(this.run, that.run)(_ append _))

  def leftMap[C](f: A => C)(implicit F: Functor[F]): TheseT[F, C, B]
  = TheseT(F.map(run)(_.leftMap(f)))

  def bimap[C, D](f: A => C, g: B => D)(implicit F: Functor[F]): TheseT[F, C, D]
  = TheseT(F.map(run)(_.bimap(f, g)))

  def traverse[G[_], AA >: A, D](g: B => G[D])(implicit F: Traverse[F], G: Applicative[G]): G[TheseT[F, AA, D]]
  = G.map(F.traverse(run)(o => Traverse[AA \&/ ?].traverse(o)(g)))(TheseT(_))


  def bitraverse[G[_], C, D](f: A => G[C], g: B => G[D])(implicit F: Traverse[F], G: Applicative[G]): G[TheseT[F, C, D]]
  = G.map(F.traverse(run)(Bitraverse[\&/].bitraverseF(f, g)))(TheseT(_: F[C \&/ D]))

  def &&&[AA >: A, C](t: TheseT[F, AA, C])(implicit M: Semigroup[AA], F: Apply[F]): TheseT[F, AA, (B, C)]
  = TheseT(F.apply2(run, t.run)(_ &&& _))

  def show(implicit SA: Show[A], SB: Show[B], F: Functor[F]): F[Cord]
  = F.map(run)(_.show)

  def foldRight[Z](z: => Z)(f: (B, => Z) => Z)(implicit F: Foldable[F]): Z
  = F.foldRight[A \&/ B, Z](run, z)((a, b) => a.foldRight(b)(f))


}


sealed abstract class TheseTInstances1 {
  implicit def TheseTInstance1[F[_]: Traverse, L]: Traverse[TheseT[F, L, ?]]
  = new Traverse[TheseT[F, L, ?]] {
    override def traverseImpl[G[_], A, B](fa: TheseT[F, L, A])(f: (A) => G[B])(implicit A: Applicative[G]) =
      fa.traverse(f)

  }

  implicit def TheseTHoist[A: Semigroup]: Hoist[TheseT[?[_], A, ?]] = new Hoist[TheseT[?[_], A, ?]] {
    override def hoist[M[_]: Monad, N[_]](f: M ~> N) = {
      new (TheseT[M, A, ?] ~> TheseT[N, A, ?]) {
        override def apply[B](fa: TheseT[M, A, B]): TheseT[N, A, B] = TheseT(f.apply(fa.run))
      }
    }

    override def liftM[G[_]: Monad, B](a: G[B]): TheseT[G, A, B] = TheseT(Monad[G].map(a)(x => \&/.That(x)))

    override implicit def apply[G[_]: Monad]: Monad[TheseT[G, A, ?]] = TheseT.theseTMonad[G, A]
  }


}

sealed abstract class TheseTInstances0 extends TheseTInstances1 {
  implicit def theseTMonad[F[_]: Monad, L: Semigroup]: Monad[TheseT[F, L, ?]]
  = new Monad[TheseT[F, L, ?]] {

    override def bind[A, B](fa: TheseT[F, L, A])(f: (A) => TheseT[F, L, B]) = fa.flatMap(f)

    override def point[A](a: => A) = TheseT(Monad[F].point(\&/.That(a)))
  }

  implicit def theseTBitraverse[F[_]: Traverse]: Bitraverse[TheseT[F, ?, ?]]
  = new Bitraverse[TheseT[F, ?, ?]] {

    override def bitraverseImpl[G[_], A, B, C, D](fab: TheseT[F, A, B])(f: (A) => G[C], g: (B) => G[D])(implicit A: Applicative[G])
    = fab.bitraverse(f, g)
  }

  implicit def theseTSemigroup[F[_]: Apply, A: Semigroup, B: Semigroup]: Semigroup[TheseT[F, A, B]] = new Semigroup[TheseT[F, A, B]] {
    override def append(f1: TheseT[F, A, B], f2: => TheseT[F, A, B]) = TheseT(Apply[F].apply2(f1.run, f2.run)(_ append _))
  }
  implicit def theseTEqual[F[_], A, B](implicit F0: Equal[F[A \&/ B]]): Equal[TheseT[F, A, B]] =
    F0.contramap((_: TheseT[F, A, B]).run)

}
sealed abstract class TheseTInstances extends TheseTInstances0

object TheseT extends TheseTInstances {
  def theseT[F[_], A, B](a: F[A \&/ B]): TheseT[F, A, B] = TheseT(a)

  def `this`[F[_]:Functor, A, B](a: F[A]): TheseT[F, A, B] = TheseT(Functor[F].map(a)(x => \&/.This(x)))
  def that[F[_]: Functor, A, B](b: F[B]): TheseT[F, A, B] = TheseT(Functor[F].map(b)(x => \&/.That(x)))
  def both[F[_]: Functor, A, B](ab: F[(A, B)]): TheseT[F, A, B] = TheseT(Functor[F].map(ab) { case (a, b) => \&/.Both(a, b)})

}

