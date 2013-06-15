package scalaz

/** A monad transformer stack yielding `(R, S1) => F[(W, A, S2)]`. */
sealed trait IndexedReaderWriterStateT[F[_], -R, W, -S1, S2, A] {
  self =>
  def run(r: R, s: S1): F[(W, A, S2)]

  /** Discards the writer component. */
  def state(r: R)(implicit F: Functor[F]): IndexedStateT[F, S1, S2, A] =
    IndexedStateT((s: S1) => F.map(run(r, s)) {
      case (w, a, s1) => (s1, a)
    })

  def map[B](f: A => B)(implicit F: Functor[F]): IndexedReaderWriterStateT[F, R, W, S1, S2, B] = new IndexedReaderWriterStateT[F, R, W, S1, S2, B] {
    def run(r: R, s: S1): F[(W, B, S2)] = F.map(self.run(r, s)) {
      case (w, a, s) => (w, f(a), s)
    }
  }

  def flatMap[B, RR <: R, S3](f: A => IndexedReaderWriterStateT[F, RR, W, S2, S3, B])(implicit F: Bind[F], W: Semigroup[W]): IndexedReaderWriterStateT[F, RR, W, S1, S3, B] = new IndexedReaderWriterStateT[F, RR, W, S1, S3, B] {
    def run(r: RR, s1: S1): F[(W, B, S3)] = {
      F.bind(self.run(r, s1)) {
        case (w1, a, s2) => {
          F.map(f(a).run(r, s2)) {
            case (w2, b, s3) => (W.append(w1, w2), b, s3)
          }
        }
      }
    }
  }
}

object IndexedReaderWriterStateT extends ReaderWriterStateTFunctions with ReaderWriterStateTInstances {
  def apply[F[_], R, W, S1, S2, A](f: (R, S1) => F[(W, A, S2)]): IndexedReaderWriterStateT[F, R, W, S1, S2, A] = new IndexedReaderWriterStateT[F, R, W, S1, S2, A] {
    def run(r: R, s: S1): F[(W, A, S2)] = f(r, s)
  }
}

trait ReaderWriterStateTFunctions {

}

trait IndexedReaderWriterStateTInstances {
  implicit def irwstFunctor[F[_], R, W, S1, S2](implicit F0: Functor[F]): Functor[({type λ[α] = IndexedReaderWriterStateT[F, R, W, S1, S2, α]})#λ] =
    new IndexedReaderWriterStateTFunctor[F, R, W, S1, S2] {
      implicit def F = F0
    }
}

trait ReaderWriterStateTInstances extends IndexedReaderWriterStateTInstances {
  implicit def rwstMonad[F[_], R, W, S](implicit W0: Monoid[W], F0: Monad[F]):
  MonadReader[({type λ[r, α]=ReaderWriterStateT[F, r, W, S, α]})#λ, R] with MonadState[({type f[s, α] = ReaderWriterStateT[F, R, W, s, α]})#f, S] with MonadListen[({type f[w, α] = ReaderWriterStateT[F, R, w, S, α]})#f, W] =
    new ReaderWriterStateTMonad[F, R, W, S] {
      implicit def F = F0
      implicit def W = W0
    }

  implicit def rwstHoist[R, W, S](implicit W0: Monoid[W]): Hoist[({type λ[α[_], β] = ReaderWriterStateT[α, R, W, S, β]})#λ] = 
    new ReaderWriterStateTHoist[R, W, S] {
      implicit def W = W0
    }

}

private[scalaz] trait IndexedReaderWriterStateTFunctor[F[_], R, W, S1, S2] extends Functor[({type λ[α]=IndexedReaderWriterStateT[F, R, W, S1, S2, α]})#λ] {
  implicit def F: Functor[F]

  override def map[A, B](fa: IndexedReaderWriterStateT[F, R, W, S1, S2, A])(f: A => B): IndexedReaderWriterStateT[F, R, W, S1, S2, B] = fa map f
}

private[scalaz] trait ReaderWriterStateTMonad[F[_], R, W, S]
  extends MonadReader[({type λ[r, α]=ReaderWriterStateT[F, r, W, S, α]})#λ, R]
  with MonadState[({type f[s, α] = ReaderWriterStateT[F, R, W, s, α]})#f, S]
  with MonadListen[({type f[w, α] = ReaderWriterStateT[F, R, w, S, α]})#f, W]
  with IndexedReaderWriterStateTFunctor[F, R, W, S, S] {
  implicit def F: Monad[F]
  implicit def W: Monoid[W]

  def bind[A, B](fa: ReaderWriterStateT[F, R, W, S, A])(f: A => ReaderWriterStateT[F, R, W, S, B]): ReaderWriterStateT[F, R, W, S, B] = fa flatMap f
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

private[scalaz] trait ReaderWriterStateTHoist[R, W, S] extends Hoist[({type λ[α[_], β] = ReaderWriterStateT[α, R, W, S, β]})#λ] {
  implicit def W: Monoid[W]
  
  def hoist[M[_], N[_]](f: M ~> N)(implicit M: Monad[M]) = new (({type λ[α] = ReaderWriterStateT[M, R, W, S, α]})#λ ~> ({type λ[α] = ReaderWriterStateT[N, R, W, S, α]})#λ) {
    def apply[A](ma: ReaderWriterStateT[M, R, W, S, A]): ReaderWriterStateT[N, R, W, S, A] = ReaderWriterStateT {
      case (r,s) => f.apply(ma.run(r,s))
    }
  }

  def liftM[M[_], A](ma: M[A])(implicit M: Monad[M]): ReaderWriterStateT[M, R, W, S, A] = {
    ReaderWriterStateT( (r,s) => M.map(ma)((W.zero, _, s)))
  }
  implicit def apply[M[_] : Monad]: Monad[({type λ[α] = ReaderWriterStateT[M, R, W, S, α]})#λ] = IndexedReaderWriterStateT.rwstMonad
}
