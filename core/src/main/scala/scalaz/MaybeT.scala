package scalaz


/**
  * monad transformer for Maybe
  */
final case class MaybeT[F[_], A](run: F[Maybe[A]]) {
  self =>
  import Maybe._

  def map[B](f: A => B)(implicit F: Functor[F]): MaybeT[F, B] = new MaybeT[F, B](mapO(_ map f))

  def mapF[B](f: A => F[B])(implicit F: Monad[F]): MaybeT[F, B] = new MaybeT[F, B](
    F.bind(self.run) {
      case Empty() => F.point(empty[B])
      case Just(z) => F.map(f(z))(b => just(b))
    }
  )

  def mapT[G[_], B](f: F[Maybe[A]] => G[Maybe[B]]): MaybeT[G, B] =
    MaybeT(f(run))

  def flatMap[B](f: A => MaybeT[F, B])(implicit F: Monad[F]): MaybeT[F, B] = new MaybeT[F, B](
    F.bind(self.run)(_.cata(f(_).run, F.point(empty)))
  )

  def flatMapF[B](f: A => F[Maybe[B]])(implicit F: Monad[F]): MaybeT[F, B] = new MaybeT[F, B](
    F.bind(self.run) {
      case Empty() => F.point(empty[B])
      case Just(z) => f(z)
    }
  )

  def foldRight[Z](z: => Z)(f: (A, => Z) => Z)(implicit F: Foldable[F]): Z = {
    F.foldRight[Maybe[A], Z](run, z)((a, b) => Foldable[Maybe].foldRight[A, Z](a, b)(f))
  }

  def traverse[G[_], B](f: A => G[B])(implicit F: Traverse[F], G: Applicative[G]): G[MaybeT[F, B]] = {
    G.map(F.traverse(run)(o => Traverse[Maybe].traverse(o)(f)))(MaybeT.apply)
  }

  def ap[B](f: => MaybeT[F, A => B])(implicit F: Monad[F]): MaybeT[F, B] =
    MaybeT(F.bind(f.run)(_.cata(ff => F.map(run)(_ map ff), F.point(empty))))

  /** Apply a function in the environment of both maybes, containing
    * both `F`s.  It is not compatible with `Monad#bind`.
    */
  def app[B](f: => MaybeT[F, A => B])(implicit F: Apply[F]): MaybeT[F, B] =
    MaybeT(F.apply2(f.run, run) {
      case (ff, aa) => maybeInstance.ap(aa)(ff)
    })

  def isJust(implicit F: Functor[F]): F[Boolean] = mapO(_.isJust)

  def isDefined(implicit F: Functor[F]): F[Boolean] = mapO(_.isJust)

  def isEmpty(implicit F: Functor[F]): F[Boolean] = mapO(_.isEmpty)

  def filter(f: A => Boolean)(implicit F: Functor[F]): MaybeT[F, A] = MaybeT(F.map(self.run) { _ filter f })

  def cata[X](just: A => X, empty: => X)(implicit F: Functor[F]): F[X] = mapO(_.cata(just, empty))

  def getOrElse(default: => A)(implicit F: Functor[F]): F[A] = mapO(_.getOrElse(default))

  /** Alias for `getOrElse`. */
  def |(default: => A)(implicit F: Functor[F]): F[A] =
    getOrElse(default)

  def getOrElseF(default: => F[A])(implicit F: Monad[F]): F[A] =
    F.bind(self.run)(_.cata(F.point(_), default))

  def orZero(implicit F0: Functor[F], M0: Monoid[A]): F[A] = getOrElse(M0.zero)

  def unary_~(implicit F0: Functor[F], M0: Monoid[A]): F[A] = orZero

  def exists(f: A => Boolean)(implicit F: Functor[F]): F[Boolean] = mapO(_.exists(f))

  def forall(f: A => Boolean)(implicit F: Functor[F]): F[Boolean] = mapO(_.forall(f))

  def orElse(a: => MaybeT[F, A])(implicit F: Monad[F]): MaybeT[F, A] =
    MaybeT(F.bind(run)(_.cata(a => F.point(just(a)), a.run)))

  def |||(a: => MaybeT[F, A])(implicit F: Monad[F]): MaybeT[F, A] =
    orElse(a)

  def toRight[E](e: => E)(implicit F: Functor[F]): EitherT[E, F, A] = EitherT(F.map(run)(_.toRight(e)))

  def toLeft[B](b: => B)(implicit F: Functor[F]): EitherT[A, F, B] = EitherT(F.map(run)(_.toLeft(b)))

  private def mapO[B](f: Maybe[A] => B)(implicit F: Functor[F]) = F.map(run)(f)
}

//
// Prioritized Implicits for type class instances
//

sealed abstract class MaybeTInstances3 {
  implicit def maybeTFunctor[F[_]](implicit F0: Functor[F]): Functor[MaybeT[F, *]] =
    new MaybeTFunctor[F] {
      implicit def F: Functor[F] = F0
    }
}

sealed abstract class MaybeTInstances2 extends MaybeTInstances3 {
  implicit def maybeTMonadError[F[_], E](implicit F0: MonadError[F, E]): MonadError[MaybeT[F, *], E] =
    new MaybeTMonadError[F, E] {
      def F = F0
    }
}

sealed abstract class MaybeTInstances1 extends MaybeTInstances2 {
  implicit def maybeTFoldable[F[_]](implicit F0: Foldable[F]): Foldable[MaybeT[F, *]] =
    new MaybeTFoldable[F] {
      implicit def F: Foldable[F] = F0
    }

  implicit def maybeTBindRec[F[_]](implicit F0: BindRec[F], F1: Monad[F]): BindRec[MaybeT[F, *]] =
    new MaybeTBindRec[F] {
      implicit def B: BindRec[F] = F0
      implicit def F: Monad[F] = F1
    }
}

sealed abstract class MaybeTInstances0 extends MaybeTInstances1 {
  implicit def maybeTMonadPlus[F[_]](implicit F0: Monad[F]): MonadPlus[MaybeT[F, *]] =
    new MaybeTMonadPlus[F] {
      implicit def F: Monad[F] = F0
    }
}

sealed abstract class MaybeTInstances extends MaybeTInstances0 {
  implicit val maybeTMonadTrans: Hoist[MaybeT] = new MaybeTHoist {}

  implicit def maybeTTraverse[F[_]](implicit F0: Traverse[F]): Traverse[MaybeT[F, *]] =
    new MaybeTTraverse[F] {
      implicit def F: Traverse[F] = F0
    }

  implicit def maybeTEqual[F[_], A](implicit F0: Equal[F[Maybe[A]]]): Equal[MaybeT[F, A]] =
    F0.contramap((_: MaybeT[F, A]).run)

  implicit def maybeTShow[F[_], A](implicit F0: Show[F[Maybe[A]]]): Show[MaybeT[F, A]] =
    Contravariant[Show].contramap(F0)(_.run)

  implicit def maybeTDecidable[F[_]](implicit F0: Divisible[F]): Decidable[MaybeT[F, *]] =
    new MaybeTDecidable[F] {
      implicit def F: Divisible[F] = F0
    }
}

object MaybeT extends MaybeTInstances {
  def maybeT[M[_]]: λ[α => M[Maybe[α]]] ~> MaybeT[M, *] =
    λ[λ[α => M[Maybe[α]]] ~> MaybeT[M, *]](
      new MaybeT(_)
    )

  def just[M[_], A](v: => A)(implicit M: Applicative[M]): MaybeT[M, A] =
    MaybeT.maybeT[M].apply[A](M.point(Maybe.just(v)))

  def empty[M[_], A](implicit M: Applicative[M]): MaybeT[M, A] =
    MaybeT.maybeT[M].apply[A](M.point(Maybe.empty))

  def monadTell[F[_], W, A](implicit MT0: MonadTell[F, W]): MonadTell[MaybeT[F, *], W] =
    new MaybeTMonadTell[F, W] {
      def MT = MT0
    }

  def monadListen[F[_], W, A](implicit ML0: MonadListen[F, W]): MonadListen[MaybeT[F, *], W] =
    new MaybeTMonadListen[F, W] {
      def MT = ML0
    }
}

//
// Implementation traits for type class instances
//

private trait MaybeTDecidable[F[_]] extends Decidable[MaybeT[F, *]] {
  implicit def F: Divisible[F]

  override final def conquer[A]: MaybeT[F, A] = MaybeT(F.conquer)

  override final def divide2[A1, A2, Z](a1: => MaybeT[F, A1], a2: => MaybeT[F, A2])(f: Z => (A1, A2)): MaybeT[F, Z] =
    MaybeT(F.divide2(a1.run, a2.run)(z => Unzip[Maybe].unzip(z.map(f))))

  override final def choose2[Z, A1, A2](a1: => MaybeT[F, A1], a2: => MaybeT[F, A2])(f: Z => A1 \/ A2): MaybeT[F, Z] =
    MaybeT(
      F.divide2(a1.run, a2.run)(_.map(f).cata(
        _.fold(a1 => (Maybe.just(a1), Maybe.empty), a2 => (Maybe.empty, Maybe.just(a2))),
        (Maybe.empty, Maybe.empty)
      ))
    )
}

private trait MaybeTFunctor[F[_]] extends Functor[MaybeT[F, *]] {
  implicit def F: Functor[F]

  override final def map[A, B](fa: MaybeT[F, A])(f: A => B): MaybeT[F, B] = fa map f
}

private trait MaybeTMonad[F[_]] extends Monad[MaybeT[F, *]] {
  implicit def F: Monad[F]

  override final def ap[A, B](fa: => MaybeT[F, A])(f: => MaybeT[F, A => B]): MaybeT[F, B] = fa ap f
  final def point[A](a: => A): MaybeT[F, A] = MaybeT[F, A](F.point(Maybe.just(a)))
  final def bind[A, B](fa: MaybeT[F, A])(f: A => MaybeT[F, B]): MaybeT[F, B] = fa flatMap f
}

private trait MaybeTBindRec[F[_]] extends BindRec[MaybeT[F, *]] with MaybeTMonad[F] {
  implicit def B: BindRec[F]

  final def tailrecM[A, B](a: A)(f: A => MaybeT[F, A \/ B]): MaybeT[F, B] =
    MaybeT(
      B.tailrecM[A, Maybe[B]](a)(a => F.map(f(a).run) {
        _.cata(_.map(Maybe.just), \/-(Maybe.empty))
      })
    )
}

private trait MaybeTFoldable[F[_]] extends Foldable.FromFoldr[MaybeT[F, *]] {
  implicit def F: Foldable[F]

  override def foldRight[A, B](fa: MaybeT[F, A], z: => B)(f: (A, => B) => B): B = fa.foldRight(z)(f)
}

private trait MaybeTTraverse[F[_]] extends Traverse[MaybeT[F, *]] with MaybeTFoldable[F] with MaybeTFunctor[F]{
  implicit def F: Traverse[F]

  def traverseImpl[G[_] : Applicative, A, B](fa: MaybeT[F, A])(f: A => G[B]): G[MaybeT[F, B]] = fa traverse f
}

private trait MaybeTHoist extends Hoist[MaybeT] {
  def liftM[G[_], A](a: G[A])(implicit G: Monad[G]): MaybeT[G, A] =
    MaybeT[G, A](G.map[A, Maybe[A]](a)((a: A) => Maybe.just(a)))

  def hoist[M[_]: Monad, N[_]](f: M ~> N) =
    λ[MaybeT[M, *] ~> MaybeT[N, *]](_ mapT f.apply)

  implicit def apply[G[_] : Monad]: Monad[MaybeT[G, *]] =
    MaybeT.maybeTMonadPlus[G]
}

private trait MaybeTMonadPlus[F[_]] extends MonadPlus[MaybeT[F, *]] with MaybeTMonad[F] {
  implicit def F: Monad[F]

  def empty[A]: MaybeT[F, A] = MaybeT(F point Maybe.empty)
  def plus[A](a: MaybeT[F, A], b: => MaybeT[F, A]): MaybeT[F, A] = a orElse b
}

private trait MaybeTMonadError[F[_], E] extends MonadError[MaybeT[F, *], E] with MaybeTMonad[F] {
  override def F: MonadError[F, E]

  override def raiseError[A](e: E) =
    MaybeT[F, A](F.map(F.raiseError[A](e))(Maybe.just))

  override def handleError[A](fa: MaybeT[F, A])(f: E => MaybeT[F, A]) =
    MaybeT[F, A](F.handleError(fa.run)(f(_).run))
}

private trait MaybeTMonadTell[F[_], W] extends MonadTell[MaybeT[F, *], W] with MaybeTMonad[F] with MaybeTHoist {
  def MT: MonadTell[F, W]

  implicit def F = MT

  def writer[A](w: W, v: A): MaybeT[F, A] =
    liftM[F, A](MT.writer(w, v))
}

private trait MaybeTMonadListen[F[_], W] extends MonadListen[MaybeT[F, *], W] with MaybeTMonadTell[F, W] {
  def MT: MonadListen[F, W]

  def listen[A](ma: MaybeT[F, A]): MaybeT[F, (A, W)] = {
    val tmp = MT.bind[(Maybe[A], W), Maybe[(A, W)]](MT.listen(ma.run)) {
      case (m, w) => m.cata(j => MT.point(Maybe.just((j, w))), MT.point(Maybe.empty))
    }

    MaybeT.maybeT[F].apply[(A, W)](tmp)
  }
}
