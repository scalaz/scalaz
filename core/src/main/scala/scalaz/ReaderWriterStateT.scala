package scalaz

sealed trait ReaderWriterStateT[F[+_], -R, +W, S, +A] {
  self =>
  def run(r: R, s: S): F[(W, A, S)]

  def state(r: R)(implicit F: Functor[F]): StateT[F, S, A] =
    StateT((s: S) => F.map(run(r, s)) {
      case (w, a, s1) => (a, s1)
    })

  def map[B, RR <: R, WW >: W](f: A => B)(implicit F: Functor[F]): ReaderWriterStateT[F, RR, WW, S, B] = new ReaderWriterStateT[F, RR, WW, S, B] {
    def run(r: RR, s: S): F[(WW, B, S)] = F.map(self.run(r, s)) {
      case (w, a, s) => (w, f(a), s)
    }
  }

  def flatMap[B, RR <: R, WW >: W](f: A => ReaderWriterStateT[F, RR, WW, S, B])(implicit F: Bind[F], W: Semigroup[WW]): ReaderWriterStateT[F, RR, WW, S, B] = new ReaderWriterStateT[F, RR, WW, S, B] {
    def run(r: RR, s: S): F[(WW, B, S)] = {
      F.bind(self.run(r, s)) {
        case (w1, a, s1) => {
          F.map(f(a).run(r, s1)) {
            case (w2, b, s2) => (W.append(w1, w2), b, s2)
          }
        }
      }
    }
  }
}

object ReaderWriterStateT extends ReaderWriterStateTFunctions with ReaderWriterStateTInstances {
  def apply[F[+_], R, W, S, A](f: (R, S) => F[(W, A, S)]): ReaderWriterStateT[F, R, W, S, A] = new ReaderWriterStateT[F, R, W, S, A] {
    def run(r: R, s: S): F[(W, A, S)] = f(r, s)
  }
}

trait ReaderWriterStateTFunctions {

}

trait ReaderWriterStateTInstances1 {
  implicit def rwstFunctor[F[+_], R, W, S](implicit F0: Functor[F]): Functor[({type λ[+α] = ReaderWriterStateT[F, R, W, S, α]})#λ] =
    new ReaderWriterStateTFunctor[F, R, W, S] {
      implicit def F = F0
    }
}

trait ReaderWriterStateTInstances0 extends ReaderWriterStateTInstances1 {
  implicit def rwstPointed[F[+_], R, W, S](implicit W0: Monoid[W], F0: Pointed[F]): Pointed[({type λ[+α] = ReaderWriterStateT[F, R, W, S, α]})#λ] =
    new ReaderWriterStateTPointed[F, R, W, S] {
      implicit def F = F0
      implicit def W = W0
    }
}

trait ReaderWriterStateTInstances extends ReaderWriterStateTInstances0 {
  implicit def rwstMonad[F[+_], R, W, S](implicit W0: Monoid[W], F0: Monad[F]):
  MonadReader[({type λ[-r, +α]=ReaderWriterStateT[F, r, W, S, α]})#λ, R] with MonadState[({type f[s, +a] = ReaderWriterStateT[F, R, W, s, a]})#f, S] =
    new ReaderWriterStateTMonadReader[F, R, W, S] {
      implicit def F = F0
      implicit def W = W0
    }
}

trait ReaderWriterStateTFunctor[F[+_], R, W, S] extends Functor[({type λ[+α]=ReaderWriterStateT[F, R, W, S, α]})#λ] {
  implicit def F: Functor[F]
  override def map[A, B](fa: ReaderWriterStateT[F, R, W, S, A])(f: A => B) = fa map f
}

trait ReaderWriterStateTPointed[F[+_], R, W, S] extends Pointed[({type λ[α]=ReaderWriterStateT[F, R, W, S, α]})#λ] with ReaderWriterStateTFunctor[F, R, W, S] {
  implicit def F: Pointed[F]
  implicit def W: Monoid[W]
  def point[A](a: => A) = ReaderWriterStateT((r, s) => F.point((W.zero, a, s)))
}

trait ReaderWriterStateTMonadReader[F[+_], R, W, S]
  extends MonadReader[({type λ[r, α]=ReaderWriterStateT[F, r, W, S, α]})#λ, R]
  with MonadState[({type f[s, a] = ReaderWriterStateT[F, R, W, s, a]})#f, S]
  with ReaderWriterStateTPointed[F, R, W, S] {
  implicit def F: Monad[F]

  def bind[A, B](fa: ReaderWriterStateT[F, R, W, S, A])(f: A => ReaderWriterStateT[F, R, W, S, B]) = fa flatMap f
  def ask: ReaderWriterStateT[F, R, W, S, R] =
    ReaderWriterStateT((r, s) => F.point((W.zero, r, s)))
  def local[A](f: (R) => R)(fa: ReaderWriterStateT[F, R, W, S, A]): ReaderWriterStateT[F, R, W, S, A] =
    ReaderWriterStateT((r, s) => fa.run(f(r), s))
  def init: ReaderWriterStateT[F, R, W, S, S] =
    ReaderWriterStateT((r, s) => F.point((W.zero, s, s)))
  def put(s: S): ReaderWriterStateT[F, R, W, S, Unit] =
    ReaderWriterStateT((r, _) => F.point((W.zero, (), s)))
}
