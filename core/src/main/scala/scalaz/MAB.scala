package scalaz

sealed trait MAB[M[_, _], A, B] {
  val v: M[A, B]

  def :->[D](g: B => D)(implicit b: Bifunctor[M]) = b.bimap(v, identity[A], g)

  def <-:[C](f: A => C)(implicit b: Bifunctor[M]) = b.bimap(v, f, identity[B])

  def >>>[C](k: M[B, C])(implicit a: Arrow[M]) = a compose (v, k)

  def <<<[C](k: M[C, A])(implicit a: Arrow[M]) = a compose (k, v)

  def fst[C](implicit a: Arrow[M]): M[(A, C), (B, C)] = a first v

  def snd[C](implicit a: Arrow[M]): M[(C, A), (C, B)] = a second v

  def ***[C, D](k: M[C, D])(implicit a: Arrow[M]) = a.compose(fst[C], a.second[C, D, B](k))

  def &&&[C](k: M[A, C])(implicit a: Arrow[M]): M[A, (B, C)] = a.compose(a.arrow(a => (a, a)), ***(k))

  def ^>>[C, D](f: C => A)(implicit a: Arrow[M]) = a.compose(a.arrow(f), v)

  def >>^[C](f: B => C)(implicit a: Arrow[M]) = a.compose(v, a.arrow(f))

  def <<^[C](f: C => A)(implicit a: Arrow[M]) = a.compose(a.arrow(f), v)

  def ^<<[C](f: B => C)(implicit a: Arrow[M]) = a.compose(v, a.arrow(f))
}

object MAB {
  def mab[M[_, _]] = new PartialWrapMAB[M, MAB] {
    def apply[A, B](a: M[A, B]) = new MAB[M, A, B] {
      val v = a
    }
  }
}
