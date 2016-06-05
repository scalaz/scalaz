package scalaz

final case class LazyEitherT[F[_], A, B](run: F[LazyEither[A, B]]) {
  import LazyEither._
  import LazyEitherT._
  import OptionT._
  import MaybeT._
  import LazyOptionT._

  def ?[X](left: => X, right: => X)(implicit F: Functor[F]): F[X] =
    F.map(run)(_.fold(_ => left, _ => right))

  def isLeft(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(_.isLeft)

  def isRight(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(_.isRight)

  def swap(implicit F: Functor[F]): F[LazyEither[B, A]] =
    F.map(run)(_.swap)

  def getOrElse(default: => B)(implicit F: Functor[F]): F[B] =
    F.map(run)(_ getOrElse default)

  /** Return the right value of this disjunction or the given default if left. Alias for `getOrElse` */
  def |(default: => B)(implicit F: Functor[F]): F[B] =
    getOrElse(default)

  def exists(f: (=> B) => Boolean)(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(_ exists f)

  def forall(f: (=> B) => Boolean)(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(_ forall f)

  def orElse(x: => LazyEitherT[F, A, B])(implicit m: Bind[F]): LazyEitherT[F, A, B] = {
    val g = run
    LazyEitherT(m.bind(g)(_.fold(
      _ => x.run
      , _ => g
    )))
  }

  /** Return this if it is a right, otherwise, return the given value. Alias for `orElse` */
  def |||(x: => LazyEitherT[F, A, B])(implicit F: Bind[F]): LazyEitherT[F, A, B] =
    orElse(x)

  def toLazyOption(implicit F: Functor[F]): LazyOptionT[F, B] =
    lazyOptionT(F.map(run)(_.toLazyOption))

  def toOption(implicit F: Functor[F]): OptionT[F, B] =
    optionT(F.map(run)(_.toOption))

  def toMaybe(implicit F: Functor[F]): MaybeT[F, B] =
    maybeT(F.map(run)(_.toMaybe))

  def toList(implicit F: Functor[F]): F[List[B]] =
    F.map(run)(_.toList)

  def toStream(implicit F: Functor[F]): F[Stream[B]] =
    F.map(run)(_.toStream)

  def map[C](f: (=> B) => C)(implicit F: Functor[F]): LazyEitherT[F, A, C] =
    lazyEitherT(F.map(run)(_ map f))

  def flatMap[C](f: (=> B) => LazyEitherT[F, A, C])(implicit M: Monad[F]): LazyEitherT[F, A, C] =
    lazyEitherT(M.bind(run)(_.fold(a => M.point(lazyLeft[C](a)), b => f(b).run)))

  def bimap[C, D](f: (=> A) => C, g: (=> B) => D)(implicit F: Functor[F]): LazyEitherT[F, C, D] =
    map(g).left.map(f)

  /** Run the given function on the left value. */
  def leftMap[C](f: (=> A) => C)(implicit F: Functor[F]): LazyEitherT[F, C, B] =
    left.map(f)

  def bitraverse[G[_], C, D](f: A => G[C], g: B => G[D])(implicit F: Traverse[F], G: Applicative[G]): G[LazyEitherT[F, C, D]] = {
    Applicative[G].map(F.traverse(run)(Bitraverse[LazyEither].bitraverseF(f, g)))(LazyEitherT(_: F[LazyEither[C, D]]))
  }

  def traverse[G[_], C](f: B => G[C])(implicit F: Traverse[F], G: Applicative[G]): G[LazyEitherT[F, A, C]] = {
    G.map(F.traverse(run)(o => LazyEither.lazyEitherInstance[A].traverse(o)(f)))(LazyEitherT(_))
  }

  def foldRight[Z](z: => Z)(f: (B, => Z) => Z)(implicit F: Foldable[F]): Z = {
    F.foldr[LazyEither[A, B], Z](run, z)(a => b => LazyEither.lazyEitherInstance[A].foldRight[B, Z](a, b)(f))
  }

  /** Apply a function in the environment of the right of this
    * disjunction.  Because it runs my `F` even when `f`'s `\/` fails,
    * it is not consistent with `ap`.
    */
  def app[C](f: => LazyEitherT[F, A, B => C])(implicit F: Apply[F]): LazyEitherT[F, A, C] = {
    // TODO check laziness
    LazyEitherT[F, A, C](F.apply2(f.run, run)((ff: LazyEither[A, B => C], aa: LazyEither[A, B]) => LazyEither.lazyEitherInstance[A].ap(aa)(ff)))
  }

  def left: LeftProjectionT[F, A, B] = new LazyEitherT.LeftProjectionT[F, A, B](LazyEitherT.this)
}

object LazyEitherT extends LazyEitherTInstances {

  def lazyEitherT[F[_], A, B](a: F[LazyEither[A, B]]): LazyEitherT[F, A, B] =
    LazyEitherT(a)

  def lazyEitherTU[FAB, AB, A0, B0](fab: FAB)(implicit
    u1: Unapply[Functor, FAB]{type A = AB},
    u2: Unapply2[Bifunctor, AB]{type A = A0; type B = B0},
    l: Leibniz.===[AB, LazyEither[A0, B0]]
  ): LazyEitherT[u1.M, A0, B0] = LazyEitherT(l.subst[u1.M](u1(fab)))

  import LazyEither._

  def lazyLeftT[F[_], A, B](a: => A)(implicit p: Applicative[F]): LazyEitherT[F, A, B] =
    lazyEitherT(p.point(lazyLeft(a)))

  def lazyRightT[F[_], A, B](b: => B)(implicit p: Applicative[F]): LazyEitherT[F, A, B] =
    lazyEitherT(p.point(lazyRight(b)))

  import Isomorphism.{IsoFunctorTemplate, IsoBifunctorTemplate, <~>, <~~>}

  def lazyEitherTLeftProjectionEIso2[F[_], E]: LazyEitherT.LeftProjectionT[F, E, ?] <~> LazyEitherT[F, E, ?] =
    new IsoFunctorTemplate[LazyEitherT.LeftProjectionT[F, E, ?], LazyEitherT[F, E, ?]] {
      def to[A](fa: LazyEitherT.LeftProjectionT[F, E, A]): LazyEitherT[F, E, A] = fa.lazyEitherT
      def from[A](ga: LazyEitherT[F, E, A]): LazyEitherT.LeftProjectionT[F, E, A] = ga.left
    }

  def lazyEitherTLeftProjectionIso2[F[_]]: LazyEitherT.LeftProjectionT[F, ?, ?] <~~> LazyEitherT[F, ?, ?] =
    new IsoBifunctorTemplate[LazyEitherT.LeftProjectionT[F, ?, ?], LazyEitherT[F, ?, ?]] {
      def to[A, B](fa: LazyEitherT.LeftProjectionT[F, A, B]): LazyEitherT[F, A, B] = fa.lazyEitherT
      def from[A, B](ga: LazyEitherT[F, A, B]): LazyEitherT.LeftProjectionT[F, A, B] = ga.left
    }

  final class LeftProjectionT[F[_], A, B](val lazyEitherT: LazyEitherT[F, A, B]) {
    import OptionT._
    import LazyOptionT._

    def getOrElse(default: => A)(implicit F: Functor[F]): F[A] =
      F.map(lazyEitherT.run)(_.left getOrElse default)

    def exists(f: (=> A) => Boolean)(implicit F: Functor[F]): F[Boolean] =
      F.map(lazyEitherT.run)(_.left exists f)

    def forall(f: (=> A) => Boolean)(implicit F: Functor[F]): F[Boolean] =
      F.map(lazyEitherT.run)(_.left forall f)

    def orElse(x: => LazyEitherT[F, A, B])(implicit m: Bind[F]): LazyEitherT[F, A, B] = {
      val g = lazyEitherT.run
      LazyEitherT(m.bind(g)((z: LazyEither[A, B]) => z.fold(
        _ => g
        , _ => x.run
      )))
    }

    def toLazyOption(implicit F: Functor[F]): LazyOptionT[F, A] =
      lazyOptionT(F.map(lazyEitherT.run)(_.left.toLazyOption))

    def toOption(implicit F: Functor[F]): OptionT[F, A] =
      optionT(F.map(lazyEitherT.run)(_.left.toOption))

    def toList(implicit F: Functor[F]): F[List[A]] =
      F.map(lazyEitherT.run)(_.left.toList)

    def toStream(implicit F: Functor[F]): F[Stream[A]] =
      F.map(lazyEitherT.run)(_.left.toStream)

    def map[C](f: (=> A) => C)(implicit F: Functor[F]): LazyEitherT[F, C, B] =
      LazyEitherT(F.map(lazyEitherT.run)(_.left map f))

    def flatMap[C](f: (=> A) => LazyEitherT[F, C, B])(implicit M: Monad[F]): LazyEitherT[F, C, B] =
      LazyEitherT(M.bind(lazyEitherT.run)(_.fold(a => f(a).run, b => M.point(LazyEither.lazyRight[C](b)))))
  }

  implicit def lazyEitherTMonadPlus[F[_], L](implicit F0: Monad[F], L: Monoid[L]): MonadPlus[LazyEitherT[F, L, ?]] =
    new LazyEitherTMonadPlus[F, L] {
      override def E = L
      override def F = F0
    }
}

sealed abstract class LazyEitherTInstances1 {
  implicit def lazyEitherTFunctor[F[_], L](implicit F0: Functor[F]): Functor[LazyEitherT[F, L, ?]] =
    new LazyEitherTFunctor[F, L] {
      implicit def F = F0
    }

  implicit def lazyEitherTLeftProjectionFunctor[F[_], L](implicit F0: Functor[F]): Functor[LazyEitherT.LeftProjectionT[F, L, ?]] =
    new IsomorphismFunctor[LazyEitherT.LeftProjectionT[F, L, ?], LazyEitherT[F, L, ?]] {
      implicit def G = lazyEitherTFunctor[F, L]

      def iso = LazyEitherT.lazyEitherTLeftProjectionEIso2[F, L]
    }

  implicit def lazyEitherTMonadError[F[_], L](implicit F0: Monad[F]): MonadError[LazyEitherT[F, L, ?], L] =
    new LazyEitherTMonadError[F, L] {
      implicit def F = F0
    }

  implicit def lazyEitherTPlus[F[_], L](implicit F0: Monad[F], L: Semigroup[L]): Plus[LazyEitherT[F, L, ?]] =
    new LazyEitherTPlus[F, L] {
      override def F = F0
      override def E = L
    }

  implicit def lazyEitherTBindRec[F[_], L](implicit F0: Monad[F], B0: BindRec[F]): BindRec[LazyEitherT[F, L, ?]] =
    new LazyEitherTBindRec[F, L] {
      implicit def F = F0
      implicit def B = B0
    }
}

sealed abstract class LazyEitherTInstances0 extends LazyEitherTInstances1 {
  implicit def lazyEitherTBifunctor[F[_]](implicit F0: Functor[F]): Bifunctor[LazyEitherT[F, ?, ?]] =
    new LazyEitherTBifunctor[F] {
      implicit def F = F0
    }

  implicit def lazyEitherTBifoldable[F[_]](implicit F0: Foldable[F]): Bifoldable[LazyEitherT[F, ?, ?]] =
    new LazyEitherTBifoldable[F] {
      implicit def F = F0
    }

  implicit def lazyEitherTLeftProjectionBifunctor[F[_]](implicit F0: Functor[F]): Bifunctor[LazyEitherT.LeftProjectionT[F, ?, ?]] =
    new IsomorphismBifunctor[LazyEitherT.LeftProjectionT[F, ?, ?], LazyEitherT[F, ?, ?]] {
      implicit def G = lazyEitherTBifunctor[F]

      def iso = LazyEitherT.lazyEitherTLeftProjectionIso2[F]
    }

  implicit def lazyEitherTMonad[F[_], L](implicit F0: Monad[F]): Monad[LazyEitherT[F, L, ?]] =
    new LazyEitherTMonad[F, L] {
      implicit def F = F0
    }

  implicit def lazyEitherTLeftProjectionMonad[F[_], L](implicit F0: Monad[F]): Monad[LazyEitherT.LeftProjectionT[F, L, ?]] =
    new IsomorphismMonad[LazyEitherT.LeftProjectionT[F, L, ?], LazyEitherT[F, L, ?]] {
      implicit def G = lazyEitherTMonad[F, L]

      def iso = LazyEitherT.lazyEitherTLeftProjectionEIso2[F, L]
    }

  implicit def lazyEitherTFoldable[F[_], L](implicit F0: Foldable[F]): Foldable[LazyEitherT[F, L, ?]] =
    new LazyEitherTFoldable[F, L] {
      implicit def F = F0
    }

  implicit def lazyEitherTLeftProjectionFoldable[F[_], L](implicit F0: Foldable[F]): Foldable[LazyEitherT.LeftProjectionT[F, L, ?]] =
    new IsomorphismFoldable[LazyEitherT.LeftProjectionT[F, L, ?], LazyEitherT[F, L, ?]] {
      implicit def G = lazyEitherTFoldable[F, L]

      def naturalTrans = LazyEitherT.lazyEitherTLeftProjectionEIso2[F, L].to
    }
}

// TODO more instances
sealed abstract class LazyEitherTInstances extends LazyEitherTInstances0 {
  implicit def lazyEitherTBitraverse[F[_]](implicit F0: Traverse[F]): Bitraverse[LazyEitherT[F, ?, ?]] =
    new LazyEitherTBitraverse[F] {
      implicit def F = F0
    }

  implicit def lazyEitherTLeftProjectionBitraverse[F[_]](implicit F0: Traverse[F]): Bitraverse[LazyEitherT.LeftProjectionT[F, ?, ?]] =
    new IsomorphismBitraverse[LazyEitherT.LeftProjectionT[F, ?, ?], LazyEitherT[F, ?, ?]] {
      implicit def G = lazyEitherTBitraverse[F]

      def iso = LazyEitherT.lazyEitherTLeftProjectionIso2[F]
    }

  implicit def lazyEitherTTraverse[F[_], L](implicit F0: Traverse[F]): Traverse[LazyEitherT[F, L, ?]] =
    new LazyEitherTTraverse[F, L] {
      implicit def F = F0
    }

  implicit def lazyEitherTLeftProjectionTraverse[F[_], L](implicit F0: Traverse[F]): Traverse[LazyEitherT.LeftProjectionT[F, L, ?]] =
    new IsomorphismTraverse[LazyEitherT.LeftProjectionT[F, L, ?], LazyEitherT[F, L, ?]] {
      implicit def G = lazyEitherTTraverse[F, L]

      def iso = LazyEitherT.lazyEitherTLeftProjectionEIso2[F, L]
    }

  implicit def lazyEitherTHoist[A]: Hoist[λ[(a[_], b) => LazyEitherT[a, A, b]]] =
    new Hoist[λ[(a[_], b) => LazyEitherT[a, A, b]]] {
      override def hoist[M[_]: Monad, N[_]](f: M ~> N) =
        new (LazyEitherT[M, A, ?] ~> LazyEitherT[N, A, ?]) {
          def apply[B](mb: LazyEitherT[M, A, B]) = LazyEitherT(f(mb.run))
        }
      override def liftM[M[_], B](mb: M[B])(implicit M: Monad[M]) =
        LazyEitherT(M.map(mb)(LazyEither.lazyRight[A].apply(_)))
      override def apply[M[_]: Monad] =
        LazyEitherT.lazyEitherTMonad[M, A]
    }
}


//
// Type class implementation traits
//

private trait LazyEitherTFunctor[F[_], E] extends Functor[LazyEitherT[F, E, ?]] {
  implicit def F: Functor[F]

  override final def map[A, B](fa: LazyEitherT[F, E, A])(f: A => B): LazyEitherT[F, E, B] = fa map (a => f(a))
}

private trait LazyEitherTMonad[F[_], E] extends Monad[LazyEitherT[F, E, ?]] with LazyEitherTFunctor[F, E] {
  implicit def F: Monad[F]

  def point[A](a: => A): LazyEitherT[F, E, A] = LazyEitherT.lazyRightT(a)

  def bind[A, B](fa: LazyEitherT[F, E, A])(f: A => LazyEitherT[F, E, B]): LazyEitherT[F, E, B] = fa flatMap (a => f(a))
}

private trait LazyEitherTPlus[F[_], E] extends Plus[LazyEitherT[F, E, ?]] {
  implicit def F: Monad[F]
  def E: Semigroup[E]

  override def plus[A](a: LazyEitherT[F, E, A], b: => LazyEitherT[F, E, A]) =
    LazyEitherT(F.bind(a.run){ r =>
      r.fold(
        l => F.map(b.run){ rr =>
          rr.fold(
            ll => LazyEither.lazyLeft(E.append(l, ll)),
            _ => rr
          )
        },
        _ => F.point(r)
      )
    })
}

private trait LazyEitherTMonadPlus[F[_], E] extends MonadPlus[LazyEitherT[F, E, ?]] with LazyEitherTMonad[F, E] with LazyEitherTPlus[F, E] {
  override def E: Monoid[E]

  override def empty[A] = LazyEitherT.lazyLeftT(E.zero)
}

private trait LazyEitherTFoldable[F[_], E] extends Foldable.FromFoldr[LazyEitherT[F, E, ?]] {
  implicit def F: Foldable[F]

  override final def foldRight[A, B](fa: LazyEitherT[F, E, A], z: => B)(f: (A, => B) => B): B = fa.foldRight(z)(f)
}

private trait LazyEitherTTraverse[F[_], E] extends Traverse[LazyEitherT[F, E, ?]] with LazyEitherTFoldable[F, E] with LazyEitherTFunctor[F, E] {
  implicit def F: Traverse[F]

  def traverseImpl[G[_]: Applicative, A, B](fa: LazyEitherT[F, E, A])(f: A => G[B]): G[LazyEitherT[F, E, B]] = fa traverse f
}

private trait LazyEitherTBifunctor[F[_]] extends Bifunctor[LazyEitherT[F, ?, ?]] {
  implicit def F: Functor[F]

  override final def bimap[A, B, C, D](fab: LazyEitherT[F, A, B])(f: A => C, g: B => D) =
    fab.map(x => g(x)).left.map(x => f(x))
}

private trait LazyEitherTBifoldable[F[_]] extends Bifoldable.FromBifoldMap[LazyEitherT[F, ?, ?]] {
  implicit def F: Foldable[F]

  override final def bifoldMap[A, B, M: Monoid](fab: LazyEitherT[F, A, B])(f: A => M)(g: B => M) =
    F.foldMap(fab.run)(Bifoldable[LazyEither].bifoldMap(_)(f)(g))
}

private trait LazyEitherTBitraverse[F[_]] extends Bitraverse[LazyEitherT[F, ?, ?]] with LazyEitherTBifunctor[F] with LazyEitherTBifoldable[F] {
  implicit def F: Traverse[F]

  def bitraverseImpl[G[_]: Applicative, A, B, C, D](fab: LazyEitherT[F, A, B])(f: A => G[C], g: B => G[D]): G[LazyEitherT[F, C, D]] =
    Applicative[G].map(F.traverse(fab.run)(Bitraverse[LazyEither].bitraverseF(f, g)))(LazyEitherT.lazyEitherT(_))
}

private trait LazyEitherTBindRec[F[_], E] extends BindRec[LazyEitherT[F, E, ?]] with LazyEitherTMonad[F, E] {
  implicit def B: BindRec[F]

  final def tailrecM[A, B](f: A => LazyEitherT[F, E, A \/ B])(a: A): LazyEitherT[F, E, B] =
    LazyEitherT(
      B.tailrecM[A, LazyEither[E, B]](a => F.map(f(a).run) {
        _.fold(e => \/-(LazyEither.lazyLeft(e)), _.map(b => LazyEither.lazyRight(b)))
      })(a)
    )
}

private trait LazyEitherTMonadError[F[_], E] extends MonadError[LazyEitherT[F, E, ?], E] with LazyEitherTMonad[F, E] {
  def raiseError[A](e: E): LazyEitherT[F, E, A] = LazyEitherT.lazyLeftT(e)
  def handleError[A](fa: LazyEitherT[F, E, A])(f: E => LazyEitherT[F, E, A]): LazyEitherT[F, E, A] = fa.left.flatMap(e => f(e))
}
