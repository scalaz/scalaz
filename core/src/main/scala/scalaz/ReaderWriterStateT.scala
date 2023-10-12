package scalaz

/** A monad transformer stack yielding `(R, S1) => F[(W, A, S2)]`. */
sealed abstract class IndexedReaderWriterStateT[R, W, S1, S2, F[_], A] {
  self =>

  def getF[S <: S1, RR <: R]: Monad[F] => F[(RR, S) => F[(W, A, S2)]]

  def run(r: R, s: S1)(implicit F: Monad[F]): F[(W, A, S2)] =
    F.join( F.map[(R, S1) => F[(W, A, S2)], F[(W, A, S2)]](getF(F))(f => f(r, s)) )

  /** Discards the writer component. */
  def state(r: R)(implicit F: Monad[F]): IndexedStateT[S1, S2, F, A] =
    IndexedStateT((s: S1) => F.map(run(r, s)) {
      case (w, a, s1) => (s1, a)
    })

  def mapT[G[_], B, WU, S2U](f: F[(W, A, S2)] => G[(WU, B, S2U)])(implicit F: Monad[F]): IndexedReaderWriterStateT[R, WU, S1, S2U, G, B] =
    IndexedReaderWriterStateT { case (r, s) => f(run(r, s)) }

  /** Calls `run` using `Monoid[S].zero` as the initial state */
  def runZero[S <: S1](r: R)(implicit F: Monad[F], S: Monoid[S]): F[(W, A, S2)] =
    run(r, S.zero)

  /** Run, discard the final state, and return the final value in the context of `F` */
  def eval(r: R, s: S1)(implicit F: Monad[F]): F[(W, A)] =
    F.map(run(r,s)) { case (w,a,s2) => (w,a) }

  /** Calls `eval` using `Monoid[S].zero` as the initial state */
  def evalZero[S <: S1](r:R)(implicit F: Monad[F], S: Monoid[S]): F[(W,A)] =
    eval(r,S.zero)

  /** Run, discard the final value, and return the final state in the context of `F` */
  def exec(r: R, s: S1)(implicit F: Monad[F]): F[(W,S2)] =
    F.map(run(r,s)){case (w,a,s2) => (w,s2)}

  /** Calls `exec` using `Monoid[S].zero` as the initial state */
  def execZero[S <: S1](r:R)(implicit F: Monad[F], S: Monoid[S]): F[(W,S2)] =
    exec(r,S.zero)

  def map[B](f: A => B)(implicit F: Functor[F]): IndexedReaderWriterStateT[R, W, S1, S2, F, B] =
    IndexedReaderWriterStateT.create[R, W, S1, S2, F, B]( (G: Monad[F]) => (r: R, s: S1) => F.map(self.run(r, s)(G))(t => (t._1, f(t._2), t._3)))

  def flatMap[B, RR <: R, S3](f: A => IndexedReaderWriterStateT[RR, W, S2, S3, F, B])(implicit F: Bind[F], W: Semigroup[W]): IndexedReaderWriterStateT[RR, W, S1, S3, F, B] =
    IndexedReaderWriterStateT.create[RR, W, S1, S3, F, B]( (G: Monad[F]) => (r: RR, s1: S1) =>
      F.bind(self.run(r, s1)(G)) {
        case (w1, a, s2) => {
          F.map(f(a).run(r, s2)(G)) {
            case (w2, b, s3) => (W.append(w1, w2), b, s3)
          }
        }
      })
}

object IndexedReaderWriterStateT extends ReaderWriterStateTInstances with ReaderWriterStateTFunctions {
  def apply[R, W, S1, S2, F[_], A](f: (R, S1) => F[(W, A, S2)]): IndexedReaderWriterStateT[R, W, S1, S2, F, A] = new IndexedReaderWriterStateT[R, W, S1, S2, F, A] {
    override def getF[S <: S1, RR <: R]: Monad[F] => F[(RR, S) => F[(W, A, S2)]] = (F: Monad[F]) => F.point((r: R, s: S) => f(r, s))
  }

  def create[R, W, S1, S2, F[_], A](f: Monad[F] => (R, S1) => F[(W,A, S2)]): IndexedReaderWriterStateT[R, W, S1, S2, F, A] = new IndexedReaderWriterStateT[R, W, S1, S2, F, A] {
    override def getF[S <: S1, RR <: R]: Monad[F] => F[(RR, S) => F[(W, A, S2)]] = (F: Monad[F]) => F.point(f(F))
  }
}

trait ReaderWriterStateTFunctions {

}

sealed abstract class IndexedReaderWriterStateTInstances1 {
  implicit def irwstFunctor[R, W, S1, S2, F[_]](implicit F0: Functor[F]): Functor[IndexedReaderWriterStateT[R, W, S1, S2, F, *]] =
    new IndexedReaderWriterStateTFunctor[F, R, W, S1, S2] {
      override def F = F0
    }
}

sealed abstract class IndexedReaderWriterStateTInstances0 extends IndexedReaderWriterStateTInstances1 {
  implicit def rwstBind[R, W, S, F[_]](implicit F0: Bind[F], W0: Semigroup[W]): Bind[ReaderWriterStateT[R, W, S, F, *]] =
    new ReaderWriterStateTBind[F, R, W, S] {
      def F = F0
      def W = W0
    }
}

sealed abstract class IndexedReaderWriterStateTInstances extends IndexedReaderWriterStateTInstances0 {
  implicit def irwstPlus[R, W, S1, S2, F[_]](implicit F0: Plus[F]): Plus[IndexedReaderWriterStateT[R, W, S1, S2, F, *]] =
    new IndexedReaderWriterStateTPlus[F, R, W, S1, S2] {
      override def F = F0
    }

  implicit def rwstBindRec[R, W, S, F[_]](implicit F0: BindRec[F], F1: Monad[F], W0: Semigroup[W]): BindRec[ReaderWriterStateT[R, W, S, F, *]] =
    new ReaderWriterStateTBindRec[F, R, W, S] {
      def F = F0
      def A = F1
      def W = W0
    }

  implicit def rwstMonadError[R, W, S, E, F[_]](implicit F0: MonadError[F, E], W0: Monoid[W]): MonadError[ReaderWriterStateT[R, W, S, F, *], E] =
    new ReaderWriterStateTMonadError[F, R, W, S, E] {
      override implicit def F: MonadError[F, E] = F0
      override implicit def W: Monoid[W] = W0
    }
}

sealed abstract class ReaderWriterStateTInstances0 extends IndexedReaderWriterStateTInstances {
  implicit def irwstPlusEmpty[R, W, S1, S2, F[_]](implicit F0: PlusEmpty[F]): PlusEmpty[IndexedReaderWriterStateT[R, W, S1, S2, F, *]] =
    new IndexedReaderWriterStateTPlusEmpty[F, R, W, S1, S2] {
      override def F = F0
    }

  implicit def rwstMonad[R, W, S, F[_]](implicit W0: Monoid[W], F0: Monad[F]):
  MonadReader[ReaderWriterStateT[R, W, S, F, *], R] with
  MonadState[ReaderWriterStateT[R, W, S, F, *], S] with
  MonadListen[ReaderWriterStateT[R, W, S, F, *], W] =
    new ReaderWriterStateTMonad[F, R, W, S] {
      override def F = F0
      override def W = W0
    }
}

abstract class ReaderWriterStateTInstances extends ReaderWriterStateTInstances0 {
  implicit def rwstMonadPlus[R, W, S, F[_]](implicit W0: Monoid[W], F0: MonadPlus[F]): MonadPlus[ReaderWriterStateT[R, W, S, F, *]] =
    new ReaderWriterStateTMonadPlus[F, R, W, S] {
      override def F = F0
      override def W = W0
    }

  implicit def rwstHoist[R, W, S](implicit W0: Monoid[W]): Hoist[({type l[α[_], β] = ReaderWriterStateT[R, W, S, α, β]})#l] =
    new ReaderWriterStateTHoist[R, W, S] {
      override def W = W0
    }

  implicit def rwstContravariantR[W, S1, S2, F[_], A]: IsContravariant[IRWST[*, W, S1, S2, F, A]] =
    IsContravariant.force[IRWST[*, W, S1, S2, F, A]]

  implicit def rwstContravariantS1[R, W, S2, F[_], A]: IsContravariant[IRWST[R, W, *, S2, F, A]] =
    IsContravariant.force[IRWST[R, W, *, S2, F, A]]

}

private trait IndexedReaderWriterStateTPlus[F[_], R, W, S1, S2] extends Plus[IndexedReaderWriterStateT[R, W, S1, S2, F, *]] {
  def F: Plus[F]

  override final def plus[A](a: IRWST[R, W, S1, S2, F, A], b: => IRWST[R, W, S1, S2, F, A]) =
    IRWST.create((G: Monad[F]) => (r: R, s: S1) => F.plus(a.run(r, s)(G), b.run(r, s)(G)))
}

private trait IndexedReaderWriterStateTPlusEmpty[F[_], R, W, S1, S2]
  extends PlusEmpty[IndexedReaderWriterStateT[R, W, S1, S2, F, *]]
  with IndexedReaderWriterStateTPlus[F, R, W, S1, S2] {
  def F: PlusEmpty[F]

  override final def empty[A] = IRWST((_, _) => F.empty)
}

private trait IndexedReaderWriterStateTFunctor[F[_], R, W, S1, S2] extends Functor[IndexedReaderWriterStateT[R, W, S1, S2, F, *]] {
  implicit def F: Functor[F]

  override final def map[A, B](fa: IndexedReaderWriterStateT[R, W, S1, S2, F, A])(f: A => B): IndexedReaderWriterStateT[R, W, S1, S2, F, B] = fa map f
}

private trait ReaderWriterStateTMonadError[F[_], R, W, S, E] extends MonadError[ReaderWriterStateT[R, W, S, F, *], E] {
  implicit def F: MonadError[F, E]
  implicit def W: Monoid[W]

  override def raiseError[A](e: E): RWST[R, W, S, F, A] =
    RWST((_, _) => F.raiseError(e))

  override def handleError[A](fa: RWST[R, W, S, F, A])(f: (E) => RWST[R, W, S, F, A]): RWST[R, W, S, F, A] =
    RWST((r, s) => F.handleError(fa.run(r, s))(f(_).run(r, s)))

  override def bind[A, B](fa: RWST[R, W, S, F, A])(f: (A) => RWST[R, W, S, F, B]): RWST[R, W, S, F, B] =
    fa flatMap f

  override def point[A](a: => A): RWST[R, W, S, F, A] =
    RWST((_, s) => F.point((W.zero, a, s)))
}

private trait ReaderWriterStateTBind[F[_], R, W, S] extends Bind[ReaderWriterStateT[R, W, S, F, *]] with IndexedReaderWriterStateTFunctor[F, R, W, S, S] {
  implicit def F: Bind[F]
  implicit def W: Semigroup[W]

  override final def bind[A, B](fa: ReaderWriterStateT[R, W, S, F, A])(f: A => ReaderWriterStateT[R, W, S, F, B]) =
    fa flatMap f
}

private trait ReaderWriterStateTBindRec[F[_], R, W, S] extends BindRec[ReaderWriterStateT[R, W, S, F, *]] with ReaderWriterStateTBind[F, R, W, S] {
  implicit def F: BindRec[F]
  implicit def A: Monad[F]

  def tailrecM[A, B](a: A)(f: A => ReaderWriterStateT[R, W, S, F, A \/ B]): ReaderWriterStateT[R, W, S, F, B] = {
    def go(r: R)(t: (W, A, S)): F[(W, A, S) \/ (W, B, S)] =
      F.map(f(t._2).run(r, t._3)) {
        case (w0, e, s0) =>
          val w1 = W.append(t._1, w0)
          e.bimap((w1, _, s0), (w1, _, s0))
      }

    ReaderWriterStateT((r, s) => F.bind(f(a).run(r, s)) {
      case (w, -\/(a0), s0) => F.tailrecM((w, a0, s0))(go(r))
      case (w, \/-(b), s0) => A.point((w, b, s0))
    })
  }
}

private abstract class ReaderWriterStateTMonadPlus[F[_], R, W, S]
  extends MonadPlus[ReaderWriterStateT[R, W, S, F, *]]
  with ReaderWriterStateTMonad[F, R, W, S]
  with IndexedReaderWriterStateTPlusEmpty[F, R, W, S, S] {
  override def F: MonadPlus[F]
}

private trait ReaderWriterStateTMonad[F[_], R, W, S]
  extends MonadReader[ReaderWriterStateT[R, W, S, F, *], R]
  with MonadState[ReaderWriterStateT[R, W, S, F, *], S]
  with MonadListen[ReaderWriterStateT[R, W, S, F, *], W]
  with ReaderWriterStateTBind[F, R, W, S] {
  implicit def F: Monad[F]
  implicit def W: Monoid[W]

  def point[A](a: => A): ReaderWriterStateT[R, W, S, F, A] =
    ReaderWriterStateT((_, s) => F.point((W.zero, a, s)))
  def ask: ReaderWriterStateT[R, W, S, F, R] =
    ReaderWriterStateT((r, s) => F.point((W.zero, r, s)))
  def local[A](f: R => R)(fa: ReaderWriterStateT[R, W, S, F, A]): ReaderWriterStateT[R, W, S, F, A] =
    ReaderWriterStateT((r, s) => fa.run(f(r), s))
  override def scope[A](k: R)(fa: ReaderWriterStateT[R, W, S, F, A]): ReaderWriterStateT[R, W, S, F, A] =
    ReaderWriterStateT((_, s) => fa.run(k, s))
  override def asks[A](f: R => A): ReaderWriterStateT[R, W, S, F, A] =
    ReaderWriterStateT((r, s) => F.point((W.zero, f(r), s)))
  def get: ReaderWriterStateT[R, W, S, F, S] =
    ReaderWriterStateT((_, s) => F.point((W.zero, s, s)))
  def put(s: S): ReaderWriterStateT[R, W, S, F, Unit] =
    ReaderWriterStateT((r, _) => F.point((W.zero, (), s)))
  override def modify(f: S => S): ReaderWriterStateT[R, W, S, F, Unit] =
    ReaderWriterStateT((r, s) => F.point((W.zero, (), f(s))))
  override def gets[A](f: S => A): ReaderWriterStateT[R, W, S, F, A] =
    ReaderWriterStateT((_, s) => F.point((W.zero, f(s), s)))
  def writer[A](w: W, v: A): ReaderWriterStateT[R, W, S, F, A] =
    ReaderWriterStateT((_, s) => F.point((w, v, s)))
  override def tell(w: W): ReaderWriterStateT[R, W, S, F, Unit] =
    ReaderWriterStateT((_, s) => F.point((w, (), s)))
  def listen[A](ma: ReaderWriterStateT[R, W, S, F, A]): ReaderWriterStateT[R, W, S, F, (A, W)] =
    ReaderWriterStateT((r, s) => F.map(ma.run(r, s)) { case (w, a, s1) => (w, (a, w), s1)})
}

private trait ReaderWriterStateTHoist[R, W, S] extends Hoist[({type l[α[_], β] = ReaderWriterStateT[R, W, S, α, β]})#l] {
  implicit def W: Monoid[W]

  def hoist[M[_], N[_]](f: M ~> N)(implicit M: Monad[M]): ReaderWriterStateT[R, W, S, M, *] ~> ReaderWriterStateT[R, W, S, N, *] =
    new (ReaderWriterStateT[R, W, S, M, *] ~> ReaderWriterStateT[R, W, S, N, *]) {
      def apply[A](ma: ReaderWriterStateT[R, W, S, M, A]): ReaderWriterStateT[R, W, S, N, A] =
        ma mapT f.apply
    }

  def liftM[M[_], A](ma: M[A])(implicit M: Monad[M]): ReaderWriterStateT[R, W, S, M, A] =
    ReaderWriterStateT( (r,s) => M.map(ma)((W.zero, _, s)))

  implicit def apply[M[_] : Monad]: Monad[ReaderWriterStateT[R, W, S, M, *]] =
    IndexedReaderWriterStateT.rwstMonad
}
