package scalaz

sealed trait MAB[M[_, _], A, B] {
  val v: M[A, B]

  def :->[D](g: B => D)(implicit b: Bifunctor[M]) = b.bimap(v, identity[A], g)

  def <-:[C](f: A => C)(implicit b: Bifunctor[M]) = b.bimap(v, f, identity[B])

  def ⋙[C](k: M[B, C])(implicit c: Category[M]) = c compose (k, v)

  def ⋘[C](k: M[C, A])(implicit c: Category[M]) = c compose (v, k)

  def first[C](implicit a: Arrow[M]): M[(A, C), (B, C)] = a first v

  def second[C](implicit a: Arrow[M]): M[(C, A), (C, B)] = a second v

  def ***[C, D](k: M[C, D])(implicit a: Arrow[M]): M[(A, C), (B, D)] = a.category.compose(a.second[C, D, B](k), first[C])

  def &&&[C](k: M[A, C])(implicit a: Arrow[M]): M[A, (B, C)] = a.category.compose(***(k), a.arrow(a => (a, a)))

  def product(implicit a: Arrow[M]): M[(A, A), (B, B)] = this *** v

  def ^>>[C](f: C => A)(implicit a: Arrow[M]) = a.category.compose(v, a.arrow(f))

  def >>^[C](f: B => C)(implicit a: Arrow[M]) = a.category.compose(a.arrow(f), v)

  def <<^[C](f: C => A)(implicit a: Arrow[M]) = a.category.compose(v, a.arrow(f))

  def ^<<[C](f: B => C)(implicit a: Arrow[M]) = a.category.compose(a.arrow(f), v)
}

trait MABs {
  implicit def mab[M[_, _], A, B](a: M[A, B]): MAB[M, A, B] = new MAB[M, A, B] {
    val v = a
  }
}
