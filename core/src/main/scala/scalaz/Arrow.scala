package scalaz

trait Arrow[F[_, _]] extends Category[F] with Arr[F] with First[F] {
  ////
  def applyInstance[C]: Apply[({type λ[α] = F[C, α]})#λ] =
    new Apply[({type λ[α] = F[C, α]})#λ] {
      def ap[A, B](fa: F[C, A])(f: F[C, (A) => B]): F[C, B] = <<<(arr((y: (A => B, A)) => y._1(y._2)), combine(f, fa))
      def map[A, B](fa: F[C, A])(f: (A) => B): F[C, B] = <<<(arr(f), fa)
    }

  def <<<[A, B, C](fbc: F[B, C], fab: F[A, B]): F[A, C] =
    compose(fbc, fab)

  def >>>[A, B, C](fab: F[A, B], fbc: F[B, C]): F[A, C] =
    compose(fbc, fab)

  def snd[A, B, C](f: F[A, B]): F[(C, A), (C, B)] = {
    def swap[X, Y] = arr[(X, Y), (Y, X)] {
      case (x, y) => (y, x)
    }

    >>>(<<<(first[A, B, C](f), swap), swap)
  }

  // ***
  def split[A, B, C, D](fab: F[A, B], fcd: F[C, D]): F[(A, C), (B, D)] =
      >>>(first[A, B, C](fab), snd[C, D, B](fcd))

  // &&&
  def combine[A, B, C](fab: F[A, B], fac: F[A, C]): F[A, (B, C)] =
      >>>(arr((a: A) => (a, a)), split(fab, fac))

  def mapfst[A, B, C](f: C => A)(fab: F[A, B]): F[C, B] =
    >>>[C, A, B](arr(f), fab)

  def mapsnd[A, B, C](f: B => C)(fab: F[A, B]): F[A, C] =
    <<<[A, B, C](arr(f), fab)
  ////
}

object Arrow {
  ////

  ////
}
