package scalaz

/** A monad transformer stack yielding `(R, S1) => F[(W, A, S2)]`. */
sealed abstract class IndexedReaderWriterStateT[F[_], -R, W, -S1, S2, A] {
  self =>

  def getF[S <: S1, RR <: R]: Monad[F] => F[(RR, S) => F[(W, A, S2)]]

  def run(r: R, s: S1)(implicit F: Monad[F]): F[(W, A, S2)] =
    F.join( F.map[(R, S1) => F[(W, A, S2)], F[(W, A, S2)]](getF(F))(f => f(r, s)) )

  /** Discards the writer component. */
  def state(r: R)(implicit F: Monad[F]): IndexedStateT[F, S1, S2, A] =
    IndexedStateT((s: S1) => F.map(run(r, s)) {
      case (w, a, s1) => (s1, a)
    })

  def mapT[G[_], B, WU, S2U](f: F[(W, A, S2)] => G[(WU, B, S2U)])(implicit F: Monad[F]): IndexedReaderWriterStateT[G, R, WU, S1, S2U, B] =
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

  def map[B](f: A => B)(implicit F: Functor[F]): IndexedReaderWriterStateT[F, R, W, S1, S2, B] =
    IndexedReaderWriterStateT.create[F, R, W, S1, S2, B]( (G: Monad[F]) => (r: R, s: S1) => F.map(self.run(r, s)(G))(t => (t._1, f(t._2), t._3)))

  def flatMap[B, RR <: R, S3](f: A => IndexedReaderWriterStateT[F, RR, W, S2, S3, B])(implicit F: Bind[F], W: Semigroup[W]): IndexedReaderWriterStateT[F, RR, W, S1, S3, B] =
    IndexedReaderWriterStateT.create[F, RR, W, S1, S3, B]( (G: Monad[F]) => (r: RR, s1: S1) =>
      F.bind(self.run(r, s1)(G)) {
        case (w1, a, s2) => {
          F.map(f(a).run(r, s2)(G)) {
            case (w2, b, s3) => (W.append(w1, w2), b, s3)
          }
        }
      })
}

object IndexedReaderWriterStateT extends ReaderWriterStateTInstances with ReaderWriterStateTFunctions {
  def apply[F[_], R, W, S1, S2, A](f: (R, S1) => F[(W, A, S2)]): IndexedReaderWriterStateT[F, R, W, S1, S2, A] = new IndexedReaderWriterStateT[F, R, W, S1, S2, A] {
    override def getF[S <: S1, RR <: R]: Monad[F] => F[(RR, S) => F[(W, A, S2)]] = (F: Monad[F]) => F.point((r: R, s: S) => f(r, s))
  }

  def create[F[_], R, W, S1, S2, A](f: Monad[F] => (R, S1) => F[(W,A, S2)]): IndexedReaderWriterStateT[F, R, W, S1, S2, A] = new IndexedReaderWriterStateT[F, R, W, S1, S2, A] {
    override def getF[S <: S1, RR <: R]: Monad[F] => F[(RR, S) => F[(W, A, S2)]] = (F: Monad[F]) => F.point(f(F))
  }
}

trait ReaderWriterStateTFunctions {

}

sealed abstract class IndexedReaderWriterStateTInstances1 {
  implicit def irwstFunctor[F[_], R, W, S1, S2](implicit F0: Functor[F]): Functor[IndexedReaderWriterStateT[F, R, W, S1, S2, ?]] =
    new IndexedReaderWriterStateTFunctor[F, R, W, S1, S2] {
      implicit def F = F0
    }
}

sealed abstract class IndexedReaderWriterStateTInstances0 extends IndexedReaderWriterStateTInstances1 {
  implicit def rwstBind[F[_], R, W, S](implicit F0: Bind[F], W0: Semigroup[W]): Bind[ReaderWriterStateT[F, R, W, S, ?]] =
    new ReaderWriterStateTBind[F, R, W, S] {
      def F = F0
      def W = W0
    }
}

sealed abstract class IndexedReaderWriterStateTInstances extends IndexedReaderWriterStateTInstances0 {
  implicit def irwstPlus[F[_], R, W, S1, S2](implicit F0: Plus[F]): Plus[IndexedReaderWriterStateT[F, R, W, S1, S2, ?]] =
    new IndexedReaderWriterStateTPlus[F, R, W, S1, S2] {
      override def F = F0
    }

  implicit def rwstBindRec[F[_], R, W, S](implicit F0: BindRec[F], F1: Monad[F], W0: Semigroup[W]): BindRec[ReaderWriterStateT[F, R, W, S, ?]] =
    new ReaderWriterStateTBindRec[F, R, W, S] {
      def F = F0
      def A = F1
      def W = W0
    }
}

sealed abstract class ReaderWriterStateTInstances0 extends IndexedReaderWriterStateTInstances {
  implicit def irwstPlusEmpty[F[_], R, W, S1, S2](implicit F0: PlusEmpty[F]): PlusEmpty[IndexedReaderWriterStateT[F, R, W, S1, S2, ?]] =
    new IndexedReaderWriterStateTPlusEmpty[F, R, W, S1, S2] {
      override def F = F0
    }

  implicit def rwstMonad[F[_], R, W, S](implicit W0: Monoid[W], F0: Monad[F]):
  MonadReader[ReaderWriterStateT[F, R, W, S, ?], R] with
  MonadState[ReaderWriterStateT[F, R, W, S, ?], S] with
  MonadListen[ReaderWriterStateT[F, R, W, S, ?], W] =
    new ReaderWriterStateTMonad[F, R, W, S] {
      implicit def F = F0
      implicit def W = W0
    }
}

abstract class ReaderWriterStateTInstances extends ReaderWriterStateTInstances0 {
  implicit def rwstMonadPlus[F[_], R, W, S](implicit W0: Monoid[W], F0: MonadPlus[F]): MonadPlus[ReaderWriterStateT[F, R, W, S, ?]] =
    new ReaderWriterStateTMonadPlus[F, R, W, S] {
      override def F = F0
      override def W = W0
    }

  implicit def rwstHoist[R, W, S](implicit W0: Monoid[W]): Hoist[λ[(α[_], β) => ReaderWriterStateT[α, R, W, S, β]]] =
    new ReaderWriterStateTHoist[R, W, S] {
      implicit def W = W0
    }

}

private trait IndexedReaderWriterStateTPlus[F[_], R, W, S1, S2] extends Plus[IndexedReaderWriterStateT[F, R, W, S1, S2, ?]] {
  def F: Plus[F]

  override final def plus[A](a: IRWST[F, R, W, S1, S2, A], b: => IRWST[F, R, W, S1, S2, A]) =
    IRWST.create((G: Monad[F]) => (r: R, s: S1) => F.plus(a.run(r, s)(G), b.run(r, s)(G)))
}

private trait IndexedReaderWriterStateTPlusEmpty[F[_], R, W, S1, S2]
  extends PlusEmpty[IndexedReaderWriterStateT[F, R, W, S1, S2, ?]]
  with IndexedReaderWriterStateTPlus[F, R, W, S1, S2] {
  def F: PlusEmpty[F]

  override final def empty[A] = IRWST((_, _) => F.empty)
}

private trait IndexedReaderWriterStateTFunctor[F[_], R, W, S1, S2] extends Functor[IndexedReaderWriterStateT[F, R, W, S1, S2, ?]] {
  implicit def F: Functor[F]

  override final def map[A, B](fa: IndexedReaderWriterStateT[F, R, W, S1, S2, A])(f: A => B): IndexedReaderWriterStateT[F, R, W, S1, S2, B] = fa map f
}

private trait ReaderWriterStateTBind[F[_], R, W, S] extends Bind[ReaderWriterStateT[F, R, W, S, ?]] with IndexedReaderWriterStateTFunctor[F, R, W, S, S] {
  implicit def F: Bind[F]
  implicit def W: Semigroup[W]

  override final def bind[A, B](fa: ReaderWriterStateT[F, R, W, S, A])(f: A => ReaderWriterStateT[F, R, W, S, B]) =
    fa flatMap f
}

private trait ReaderWriterStateTBindRec[F[_], R, W, S] extends BindRec[ReaderWriterStateT[F, R, W, S, ?]] with ReaderWriterStateTBind[F, R, W, S] {
  implicit def F: BindRec[F]
  implicit def A: Monad[F]

  def tailrecM[A, B](f: A => ReaderWriterStateT[F, R, W, S, A \/ B])(a: A): ReaderWriterStateT[F, R, W, S, B] = {
    def go(r: R)(t: (W, A, S)): F[(W, A, S) \/ (W, B, S)] =
      F.map(f(t._2).run(r, t._3)) {
        case (w0, e, s0) =>
          val w1 = W.append(t._1, w0)
          e.bimap((w1, _, s0), (w1, _, s0))
      }

    ReaderWriterStateT((r, s) => F.bind(f(a).run(r, s)) {
      case (w, -\/(a0), s0) => F.tailrecM(go(r))(w, a0, s0)
      case (w, \/-(b), s0) => A.point((w, b, s0))
    })
  }
}

private abstract class ReaderWriterStateTMonadPlus[F[_], R, W, S]
  extends MonadPlus[ReaderWriterStateT[F, R, W, S, ?]]
  with ReaderWriterStateTMonad[F, R, W, S]
  with IndexedReaderWriterStateTPlusEmpty[F, R, W, S, S] {
  override def F: MonadPlus[F]
}

private trait ReaderWriterStateTMonad[F[_], R, W, S]
  extends MonadReader[ReaderWriterStateT[F, R, W, S, ?], R]
  with MonadState[ReaderWriterStateT[F, R, W, S, ?], S]
  with MonadListen[ReaderWriterStateT[F, R, W, S, ?], W]
  with ReaderWriterStateTBind[F, R, W, S] {
  implicit def F: Monad[F]
  implicit def W: Monoid[W]

  def point[A](a: => A): ReaderWriterStateT[F, R, W, S, A] =
    ReaderWriterStateT((_, s) => F.point((W.zero, a, s)))
  def ask: ReaderWriterStateT[F, R, W, S, R] =
    ReaderWriterStateT((r, s) => F.point((W.zero, r, s)))
  def local[A](f: R => R)(fa: ReaderWriterStateT[F, R, W, S, A]): ReaderWriterStateT[F, R, W, S, A] =
    ReaderWriterStateT((r, s) => fa.run(f(r), s))
  override def scope[A](k: R)(fa: ReaderWriterStateT[F, R, W, S, A]): ReaderWriterStateT[F, R, W, S, A] =
    ReaderWriterStateT((_, s) => fa.run(k, s))
  override def asks[A](f: R => A): ReaderWriterStateT[F, R, W, S, A] =
    ReaderWriterStateT((r, s) => F.point((W.zero, f(r), s)))
  def init: ReaderWriterStateT[F, R, W, S, S] =
    ReaderWriterStateT((_, s) => F.point((W.zero, s, s)))
  def get = init
  def put(s: S): ReaderWriterStateT[F, R, W, S, Unit] =
    ReaderWriterStateT((r, _) => F.point((W.zero, (), s)))
  override def modify(f: S => S): ReaderWriterStateT[F, R, W, S, Unit] =
    ReaderWriterStateT((r, s) => F.point((W.zero, (), f(s))))
  override def gets[A](f: S => A): ReaderWriterStateT[F, R, W, S, A] =
    ReaderWriterStateT((_, s) => F.point((W.zero, f(s), s)))
  def writer[A](w: W, v: A): ReaderWriterStateT[F, R, W, S, A] =
    ReaderWriterStateT((_, s) => F.point((w, v, s)))
  override def tell(w: W): ReaderWriterStateT[F, R, W, S, Unit] =
    ReaderWriterStateT((_, s) => F.point((w, (), s)))
  def listen[A](ma: ReaderWriterStateT[F, R, W, S, A]): ReaderWriterStateT[F, R, W, S, (A, W)] =
    ReaderWriterStateT((r, s) => F.map(ma.run(r, s)) { case (w, a, s1) => (w, (a, w), s1)})
}

private trait ReaderWriterStateTHoist[R, W, S] extends Hoist[λ[(α[_], β) => ReaderWriterStateT[α, R, W, S, β]]] {
  implicit def W: Monoid[W]

  def hoist[M[_], N[_]](f: M ~> N)(implicit M: Monad[M]) =
    new (ReaderWriterStateT[M, R, W, S, ?] ~> ReaderWriterStateT[N, R, W, S, ?]) {
      def apply[A](ma: ReaderWriterStateT[M, R, W, S, A]): ReaderWriterStateT[N, R, W, S, A] =
        ma.mapT(f)
    }

  def liftM[M[_], A](ma: M[A])(implicit M: Monad[M]): ReaderWriterStateT[M, R, W, S, A] =
    ReaderWriterStateT( (r,s) => M.map(ma)((W.zero, _, s)))

  implicit def apply[M[_] : Monad]: Monad[ReaderWriterStateT[M, R, W, S, ?]] =
    IndexedReaderWriterStateT.rwstMonad
}
